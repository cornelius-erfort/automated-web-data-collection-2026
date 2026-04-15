library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)
library(xml2)
library(jsonlite)
library(here)

# Set Greens specific variables
party_url <- "https://www.gruene-bundestag.de/presse/"
party_id <- 772
country_name <- "Germany"
force_rescrape <- TRUE

# Add wayback machine base URL
wayback_base <- "https://web.archive.org/web/"

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from a page
extract_gruene_press_links <- function(page_url, is_wayback = FALSE) {
  tryCatch({
    page <- read_html(page_url)
    
    # Initialize empty vector for hrefs
    hrefs <- character()
    
    # Pattern 1: basicTeaser with nested structure
    teasers <- page %>% html_nodes("article.basicTeaser")
    for (teaser in teasers) {
      type <- teaser %>% 
        html_node(".basicTeaser__superHeadline") %>% 
        html_text(trim = TRUE)
      
      if (type %in% c("Pressemitteilung", "Statement")) {
        # Try to get href from wrapper link first
        href <- teaser %>% 
          html_node("a.basicTeaser__wrapper") %>% 
          html_attr("href")
        
        if (is.na(href)) {
          # Fallback to basicTeaser__content href
          href <- teaser %>% 
            html_node(".basicTeaser__content") %>% 
            html_attr("href")
        }
        
        if (!is.na(href)) {
          hrefs <- c(hrefs, href)
        }
      }
    }
    
    # Pattern 2: current-list__link with title attribute
    links <- page %>% html_nodes(".current-list__link")
    for (link in links) {
      title <- link %>% html_attr("title")
      href <- link %>% html_attr("href")
      if (!is.na(href) && 
          (!is.na(title) && (startsWith(title, "Pressemitteilung") || startsWith(title, "Statement")))) {
        hrefs <- c(hrefs, href)
      }
    }
    
    # Pattern 3: current-list__item with data-type="pressemitteilung"
    items <- page %>% html_nodes('.current-list__item[data-type="pressemitteilung"]')
    for (item in items) {
      link <- item %>% html_node(".current-list__link")
      if (!is.null(link)) {
        href <- link %>% html_attr("href")
        if (!is.na(href)) {
          hrefs <- c(hrefs, href)
        }
      }
    }
    
    # If no hrefs were found, handle based on source
    if (length(hrefs) == 0) {
      if (is_wayback) {
        # For wayback pages, this is an error
        stop("No press release links found in wayback page: ", page_url)
      } else {
        # For live pages, just warn and return empty
        warning("No press release links found in live page: ", page_url)
        return(character())
      }
    }
    
    message("Found ", length(hrefs), " links")
    message("Sample links:")
    message(paste(utils::head(hrefs), collapse = "\n"))
    
    # Clean up wayback URLs if needed
    urls <- if (is_wayback) {
      # For wayback URLs, ensure they have the wayback_base prefix
      hrefs <- ifelse(
        startsWith(hrefs, wayback_base),
        hrefs,
        paste0(wayback_base, hrefs)
      )
      gsub("^/web/[0-9]+/", "", hrefs)
    } else {
      # For live URLs, make them absolute if needed
      ifelse(
        startsWith(hrefs, "/"),
        paste0("https://www.gruene-bundestag.de", hrefs),
        hrefs
      )
    }
    
    # Clean up URLs by removing any duplicates
    urls <- unique(urls)
    
    message("After processing, kept ", length(urls), " unique URLs")
    
    return(urls)
    
  }, error = function(e) {
    # Re-throw the error for wayback pages
    if (is_wayback) {
      stop(e$message)
    } else {
      warning("Error extracting links from ", page_url, ": ", e$message)
      return(character())
    }
  })
}

# Function to load existing pagination data
load_pagination_data <- function(party_id) {
  filename <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  if (file.exists(filename)) {
    message("Found existing pagination data for party ID ", party_id)
    df <- readRDS(filename)
    message("Loaded ", nrow(df), " existing press release URLs")
    return(df)
  }
  return(NULL)
}

# Function to save pagination data
save_pagination_data <- function(df, party_id) {
  filename <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  
  df <- filter(df, !duplicated(url)) # Remove duplicates
  saveRDS(df, filename)
  message("Saved ", nrow(df), " URLs to ", filename)
}

# Function to get wayback cookies
get_wayback_cookies <- function(url) {
  calendar_url <- paste0("https://web.archive.org/web/*/", url)
  
  tryCatch({
    # Make initial request to get cookies
    response <- GET(calendar_url)
    cookies <- cookies(response)
    
    # Convert cookies to a string format for headers
    cookie_string <- paste(
      names(cookies), 
      cookies, 
      sep = "=", 
      collapse = "; "
    )
    
    # Create headers
    headers <- c(
      "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:136.0) Gecko/20100101 Firefox/136.0",
      "Accept" = "*/*",
      "Accept-Language" = "en-US,en;q=0.5",
      "Accept-Encoding" = "gzip, deflate, br",
      "Connection" = "keep-alive",
      "Cookie" = cookie_string,
      "Referer" = calendar_url
    )
    
    return(headers)
  }, error = function(e) {
    warning("Error getting wayback cookies: ", e$message)
    return(NULL)
  })
}

# Modified function to get available years and months using cookies
get_wayback_availability <- function(url, headers = NULL) {
  if (is.null(headers)) {
    headers <- get_wayback_cookies(url)
  }
  
  sparkline_api <- paste0("https://web.archive.org/__wb/sparkline?output=json&url=", 
                         utils::URLencode(url), 
                         "&collection=web")
  
  tryCatch({
    response <- GET(sparkline_api, 
                   add_headers(.headers = headers))
    
    if (status_code(response) != 200) {
      warning("Failed to get wayback availability, status code: ", 
              status_code(response))
      return(list())
    }
    
    data <- fromJSON(rawToChar(response$content))
    years_data <- data$years
    
    # Create a list to store all available dates
    available_dates <- list()
    
    # Process each year
    for (year in names(years_data)) {
      months <- years_data[[year]]
      for (month_idx in seq_along(months)) {
        if (months[month_idx] > 0) {
          month <- sprintf("%02d", month_idx)
          available_dates[[length(available_dates) + 1]] <- list(
            year = year,
            month = month
          )
        }
      }
    }
    
    return(available_dates)
  }, error = function(e) {
    warning("Error getting wayback availability: ", e$message)
    return(list())
  })
}

# Modified function to get available days using cookies
get_wayback_days <- function(url, year, month, wayback_headers = NULL) {
  if (is.null(wayback_headers)) {
    wayback_headers <- get_wayback_cookies(url)
  }
  
  calendar_api <- paste0("https://web.archive.org/__wb/calendarcaptures/2?url=", 
                        utils::URLencode(url),
                        "&date=", year, month,
                        "&groupby=day")
  
  tryCatch({
    response <- GET(calendar_api,
                   add_headers(.headers = wayback_headers))
    
    if (status_code(response) != 200) {
      warning("Failed to get wayback days, status code: ", 
              status_code(response))
      return(character())
    }
    
    days <- fromJSON(rawToChar(response$content))$items[, 1]
    return(days)
  }, error = function(e) {
    warning("Error getting wayback days: ", e$message)
    return(character())
  })
}

# Function to get the latest snapshot for a specific date
get_latest_snapshot <- function(url, date, wayback_headers = NULL) {
  if (is.null(wayback_headers)) {
    wayback_headers <- get_wayback_cookies(url)
  }
  
  captures_api <- paste0("https://web.archive.org/__wb/calendarcaptures/2?url=", 
                        utils::URLencode(url),
                        "&date=", date %>% substr(1,4), "&groupby=day")
  
  tryCatch({
    response <- GET(captures_api,
                   add_headers(.headers = wayback_headers))
    
    if (status_code(response) != 200) {
      if (status_code(response) == 429) {  # Too Many Requests
        message("Rate limit hit, sleeping for 5 minutes...")
        Sys.sleep(300)  # Sleep for 5 minutes
        return(NULL)
      }
      warning("Failed to get snapshots, status code: ", status_code(response))
      return(NULL)
    }
    
    data <- fromJSON(rawToChar(response$content))
    if (length(data$items) > 0) {
      # Get items with status 200
      success_items <- data$items[data$items[,2] == 200,]
      if (length(success_items) > 0) {
        # Get the last successful snapshot of the day
        timestamp <- sprintf("%06d", tail(success_items[1], 1))
        return(paste0(wayback_base, date, timestamp, "/", url))
      }
    }
    return(NULL)
  }, error = function(e) {
    warning("Error getting snapshot times: ", e$message)
    return(NULL)
  })
}

# Function to get weekly snapshots from available days
get_weekly_snapshots <- function(days, year, month) {
  # Create full dates by combining year, month, and days
  dates <- as.Date(paste0(year, "-", month, "-", sprintf("%02d", days)))
  
  # Group by week and get the first day of each week
  weekly_dates <- dates %>%
    as_tibble() %>%
    mutate(week = floor_date(value, unit = "week")) %>%
    group_by(week) %>%
    summarise(sample_date = first(value)) %>%
    pull(sample_date)
  
  # Convert back to YYYYMMDD format
  format(weekly_dates, "%Y%m%d")
}

# Add rate limiting function
rate_limit_sleep <- function() {
  Sys.sleep(4)  # Sleep for 4 seconds to stay under 15 requests per minute
}

# Modified scraper function
scrape_gruene_press_releases <- function(base_url = party_url, 
                                       force_rescrape = FALSE,
                                       use_wayback = TRUE) {
  # Initialize empty dataframe for all links
  all_links_df <- tibble(
    timestamp = as.POSIXct(character()),
    url = character(),
    source = character(),
    source_url = character()
  )
  
  # Load existing pagination data if any
  existing_df <- load_pagination_data(party_id)
  if (!is.null(existing_df)) {
    message("Found existing pagination data with ", nrow(existing_df), " URLs")
    all_links_df <- bind_rows(all_links_df, existing_df)
  }
  
  # Only scrape new data if force_rescrape is TRUE or we don't have existing data
  if (force_rescrape || is.null(existing_df)) {
    # Initialize all_links_df with existing data if any
    all_links_df <- if (!is.null(existing_df)) existing_df else tibble(
      timestamp = as.POSIXct(character()),
      url = character(),
      source = character(),
      source_url = character()
    )
    
    # Scrape current live page
    message("Scraping current live page...")
    links <- extract_gruene_press_links(base_url, is_wayback = FALSE)
    if (length(links) > 0) {
      live_df <- tibble(
        timestamp = Sys.time(),
        url = links,
        source = "live",
        source_url = base_url
      )
      all_links_df <- bind_rows(all_links_df, live_df)
    }
    
    # Get wayback cookies once at the start
    wayback_headers <- if (use_wayback) {
      get_wayback_cookies(base_url)
    } else {
      NULL
    }
    
    # Scrape wayback machine if enabled
    if (use_wayback && !is.null(wayback_headers)) {
      message("Getting Wayback Machine availability...")
      rate_limit_sleep()
      available_dates <- get_wayback_availability(base_url, wayback_headers)
      
      # Get existing wayback timestamps
      existing_wayback_timestamps <- if (!is.null(existing_df)) {
        existing_df %>%
          filter(source == "wayback") %>%
          pull(timestamp) %>%
          format("%Y%m%d")
      } else {
        character(0)
      }
      
      for (date_info in available_dates) {
        year <- date_info$year
        month <- date_info$month
        
        message(sprintf("Getting days for %s-%s", year, month))
        rate_limit_sleep()
        days <- get_wayback_days(base_url, year, month, wayback_headers)
        
        # Get only weekly snapshots
        weekly_days <- get_weekly_snapshots(days, year, month)
        
        # Filter out weeks we already have
        new_weekly_days <- weekly_days[!weekly_days %in% existing_wayback_timestamps]
        
        if (length(new_weekly_days) == 0) {
          message(sprintf("Skipping %s-%s, all weeks already processed", year, month))
          next
        }
        
        message(sprintf("Processing %d new weeks for %s-%s", 
                       length(new_weekly_days), year, month))
        
        for (day in new_weekly_days) {
          message(sprintf("Getting snapshot for week of %s", day))
          rate_limit_sleep()
          
          snapshot_url <- get_latest_snapshot(base_url, day, wayback_headers)
          if (!is.null(snapshot_url)) {
            rate_limit_sleep()
            
            links <- extract_gruene_press_links(snapshot_url, is_wayback = TRUE)
            if (length(links) > 0) {
              wayback_df <- tibble(
                timestamp = as.POSIXct(day, format = "%Y%m%d"),
                url = links,
                source = "wayback",
                source_url = snapshot_url
              )
              all_links_df <- bind_rows(all_links_df, wayback_df)
              
              # Save progress after each successful snapshot
              save_pagination_data(all_links_df, party_id)
            }
          }
        }
      }
    }
  }
  
  # Deduplicate links while keeping the earliest timestamp for each URL
  pagination_df <- all_links_df %>%
    arrange(url, timestamp) %>%  # Sort by URL and timestamp
    group_by(url) %>%
    slice(1) %>%  # Keep the earliest record for each URL
    ungroup()
  
  if (nrow(pagination_df) > 0) {
    save_pagination_data(pagination_df, party_id)
    message("Saved ", nrow(pagination_df), " unique URLs from ", 
            n_distinct(all_links_df$source), " sources")
  } else {
    message("No links found")
  }
  
  return(pagination_df)
}

# Function to get the earliest snapshot for a specific date
get_earliest_snapshot <- function(url, date, wayback_headers = NULL) {
  if (is.null(wayback_headers)) {
    wayback_headers <- get_wayback_cookies(url)
  }
  
  captures_api <- paste0("https://web.archive.org/__wb/calendarcaptures/2?url=", 
                        utils::URLencode(url),
                        "&date=", date %>% substr(1,4), "&groupby=day")
  
  tryCatch({
    response <- GET(captures_api,
                   add_headers(.headers = wayback_headers))
    
    if (status_code(response) != 200) {
      warning("Failed to get snapshots, status code: ", status_code(response))
      return(NULL)
    }
    
    data <- fromJSON(rawToChar(response$content))
    if (length(data$items) > 0) {
      # Get items with status 200
      success_items <- data$items[data$items[,2] == 200,]
      if (length(success_items) > 0) {
        # Get the first successful snapshot of the day
        timestamp <- sprintf("%06d", head(success_items[1] %>% as.numeric(), 1))
        return(paste0(wayback_base, date, timestamp, "/", url))
      }
    }
    return(NULL)
  }, error = function(e) {
    warning("Error getting snapshot times: ", e$message)
    return(NULL)
  })
}

# Function to get the earliest available year and month
get_earliest_year_month <- function(url, wayback_headers = NULL) {
  if (is.null(wayback_headers)) {
    wayback_headers <- get_wayback_cookies(url)
  }
  
  sparkline_api <- paste0("https://web.archive.org/__wb/sparkline?output=json&url=", 
                          utils::URLencode(url), 
                          "&collection=web")
  
  tryCatch({
    response <- GET(sparkline_api, add_headers(.headers = wayback_headers))
    
    if (status_code(response) != 200) {
      warning("Failed to get wayback availability, status code: ", status_code(response))
      return(NULL)
    }
    
    data <- fromJSON(rawToChar(response$content))
    years_data <- data$years
    
    # Find the earliest year and month with available snapshots
    for (year in sort(names(years_data))) {
      months <- years_data[[year]]
      for (month_idx in seq_along(months)) {
        if (months[month_idx] > 0) {
          month <- sprintf("%02d", month_idx)
          return(list(year = year, month = month))
        }
      }
    }
    
    return(NULL)
  }, error = function(e) {
    warning("Error getting wayback availability: ", e$message)
    return(NULL)
  })
}

# Function to process Greens press releases
process_press_releases_gruene <- function(pagination_df) {
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  
  if (file.exists(output_file)) {
    results <- read_csv(output_file)
    message("Loaded ", nrow(results), " existing press releases from CSV")
  } else {
    results <- tibble(
      title = character(),
      date = as.Date(character()),
      text = character(),
      url = character(),
      party = character(),
      country = character(),
      year = integer(),
      parlgov_party_id = integer(),
      date_collected = as.Date(character()),
      source = character(),
      snapshot = character()  # New column for earliest snapshot URL
    )
  }
  
  remaining_urls <- setdiff(pagination_df$url, results$url)
  pagination_df <- pagination_df[pagination_df$url %in% remaining_urls, ]
  total_remaining <- nrow(pagination_df)
  
  url_delete_count <- 0
  
  for (i in 1:total_remaining) {
    message("Processing ", length(setdiff(pagination_df$url, results$url)), " remaining press releases...")

    row <- pagination_df[i-url_delete_count,]
    url <- row$url
    # Skip if url is NA
    if (is.na(url)) {
      message("Skipping NA URL")
      next
    }
    source <- row$source
    
    message("Processing URL: ", url, " (", source, ")")
    
    # Get the earliest available year and month
    earliest_year_month <- if (source == "wayback") {
      get_earliest_year_month(url)
    } else {
      NULL
    }
    
    # Get the earliest snapshot URL
    snapshot_url <- if (!is.null(earliest_year_month)) {
      get_earliest_snapshot(url, paste0(earliest_year_month$year, earliest_year_month$month))
    } else {
      NA_character_
    }
    
    if (is.null(snapshot_url)) {
      message("Failed to access URL: ", fetch_url)
      # Remove the url from pagination_df
      pagination_df <- pagination_df[pagination_df$url != url, ]
      url_delete_count <- url_delete_count + 1
      next
    }
    
    # Use the snapshot URL if available, otherwise use the original URL
    fetch_url <- if (!is.na(snapshot_url)) snapshot_url else url
    
    
    
    success <- FALSE
    page <- NULL
    
    tryCatch({
      Sys.sleep(2) # Rate limiting
      page <- read_html(fetch_url)
      success <- TRUE
      message("Successfully accessed ", source, " URL")
    }, error = function(e) {
      message("Failed to access URL: ", e$message)
      
    })
    
    if (!success || is.null(page)) {
      message("Failed to access URL: ", fetch_url)
      # Remove the url from pagination_df
      pagination_df <- pagination_df[pagination_df$url != url, ]
      next
    }
    
    tryCatch({
      # Extract title from h1 or h2 (multiple patterns)
      title <- page %>%
        html_node("h1.articleHeader__title, h1, h2") %>%
        html_text() %>%
        str_trim()
      
      # Extract date (multiple patterns)
      date_text <- page %>%
        html_nodes(".meta__date, .release-date time, .articleHeader__superHeadline, time[datetime]") %>%
        {
          # Try datetime attribute first, then fallback to text content
          datetime_attr <- html_attr(., "datetime")
          if (!is.null(datetime_attr) && !all(is.na(datetime_attr))) {
            datetime_attr[!is.na(datetime_attr)][1]
          } else {
            html_text(.) %>% str_trim() %>% .[1]
          }
        }
      
      # Clean up date text (handle different formats)
      date_text <- str_extract(date_text, "\\d{2,4}.\\d{2}.\\d{2,4}")
      
      # Parse the date (handle different formats)
      date <- tryCatch({
        as.Date(date_text)
      }, error = function(e) {
        as.Date(date_text, format = "%d.%m.%Y")
      })
      
      # Extract text content from all paragraphs (multiple patterns)
      text_elements <- page %>%
        html_nodes(".frame-type-text p, .basicTeaser__main, .co__rteContent p, .text.frame p") %>%
        html_text() %>%
        str_trim() %>%
        .[nchar(.) > 0]  # Remove empty paragraphs
      
      text <- paste(text_elements, collapse = "\n\n")
      
      # Log the extracted content
      message("\nExtracted content for: ", url)
      message("----------------------------------------")
      message("Title: ", if(is.na(title)) "NA" else title)
      message("Date: ", if(is.na(date)) "NA" else as.character(date))
      message("Text preview (first 150 chars): ", 
              if(is.na(text)) "NA" else substr(text, 1, 150), "...")
      message("Source: ", row$source)
      message("Snapshot URL: ", if(is.na(snapshot_url)) "NA" else snapshot_url)
      message("----------------------------------------\n")
      
      # Always add the row, even with missing values
      new_row <- tibble(
        title = title,
        date = date,
        text = text,
        url = url,
        party = "B90/Gru",
        country = country_name,
        year = if (!is.na(date)) year(date) else NA_integer_,
        parlgov_party_id = party_id,
        date_collected = Sys.Date(),
        source = row$source,
        snapshot = snapshot_url
      )
      
      results <- bind_rows(results, new_row) %>% arrange(desc(date))
      
      # Remove pages with redirects to main page
      results <- filter(results, title != "Presse" | is.na(title))
      
      # Remove duplicate snapshots
      results <- filter(results, !duplicated(url))
      write_csv(results, output_file)
      
      # Log what we found and what we missed
      missing_fields <- character()
      if (is.na(title)) missing_fields <- c(missing_fields, "title")
      if (is.na(date)) missing_fields <- c(missing_fields, "date")
      if (is.na(text)) missing_fields <- c(missing_fields, "text")
      
      if (length(missing_fields) > 0) {
        message("Fields missing: ", paste(missing_fields, collapse = ", "))
      } else {
        message("All fields successfully extracted")
      }
      
      message("Total press releases processed: ", nrow(results))
      
    }, error = function(e) {
      message("Error processing URL ", url, ": ", e$message)
    })
    
    # Rate limiting
    rate_limit_sleep()
  }
  
  return(results)
}

# Main execution
message("Starting B90/Grüne press release scraper...")

# Start fresh scrape if force_rescrape is TRUE, otherwise try to continue from existing data
gruene_pagination <- if (force_rescrape) {
  message("Force rescrape enabled, starting fresh scrape")
  scrape_gruene_press_releases(force_rescrape = TRUE)
} else {
  tryCatch({
    existing_data <- load_pagination_data(party_id)
    if (!is.null(existing_data)) {
      message("Found existing pagination data, continuing from last page")
      scrape_gruene_press_releases(force_rescrape = FALSE)
    } else {
      message("No existing data found, starting fresh scrape")
      scrape_gruene_press_releases(force_rescrape = TRUE)
    }
  }, error = function(e) {
    message("Error during scraping: ", e$message)
    existing_data <- load_pagination_data(party_id)
    if (!is.null(existing_data)) {
      message("Loaded existing pagination data as fallback")
      return(existing_data)
    } else {
      message("No existing pagination data found")
      return(NULL)
    }
  })
}

gruene_pagination <- readRDS("meta-data/pagination_772.rds")
gruene_pagination <- filter(gruene_pagination, !duplicated(url))

saveRDS(gruene_pagination, "meta-data/pagination_772.rds")

# Order with source live at the top
gruene_pagination <- gruene_pagination %>%
  arrange((source))


if (!is.null(gruene_pagination)) {
  message("Processing press releases...")
  press_releases <- process_press_releases_gruene(gruene_pagination)
  
  # Final deduplication
  message("Performing final deduplication...")
  initial_rows <- nrow(press_releases)
  
  press_releases <- press_releases %>%
    arrange(url, desc(date)) %>%
    distinct(url, .keep_all = TRUE)
  
  press_releases <- filter(press_releases, !duplicated(url))
  
  press_releases
  
  final_rows <- nrow(press_releases)
  
  if (initial_rows != final_rows) {
    message(sprintf("Removed %d duplicate entries", initial_rows - final_rows))
  } else {
    message("No duplicates found")
  }
  
  # Save final results
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  
  press_releases <- filter(press_releases, !duplicated(url))
  write_csv(press_releases, output_file)
  
  message(sprintf("Saved %d press releases to %s", nrow(press_releases), output_file))
} else {
  message("No pagination data available, cannot process press releases")
} 

# Make snapshot url to url and remove
output_file <- file.path("press_release_data", 
                         paste0(tolower(country_name), "_", party_id, ".csv"))
press_releases <- read_csv(output_file)
press_releases$url[!is.na(press_releases$snapshot)] <- press_releases$snapshot[!is.na(press_releases$snapshot)]
press_releases <- press_releases %>% select(-snapshot)

write_csv(press_releases, output_file)

