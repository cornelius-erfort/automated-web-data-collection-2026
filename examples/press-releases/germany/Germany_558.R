library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)
library(xml2)
library(jsonlite)
library(here)

# Set SPD specific variables
party_url <- "https://www.spd.de/service/pressemitteilungen"
party_id <- 558
country_name <- "Germany"
force_rescrape <- FALSE

# Function to get the last page number for SPD pagination
get_spd_last_page <- function(base_url) {
  tryCatch({
    page <- read_html(base_url)
    last_page <- page %>%
      html_nodes(".pagination__control") %>%
      html_text() %>%
      str_trim() %>%
      .[!grepl("…", .)] %>%
      as.numeric() %>%
      max(na.rm = TRUE)
    
    return(last_page)
  }, error = function(e) {
    warning("Error getting last page number: ", e$message)
    return(NULL)
  })
}

# Function to extract press release links from a page
extract_spd_press_links <- function(page_url) {
  tryCatch({
    page <- read_html(page_url)
    links <- page %>%
      html_nodes(".news-teaser__more-link a") %>%
      html_attr("href") %>%
      unique()
    
    # Convert relative URLs to absolute
    links <- paste0("https://www.spd.de", links)
    
    return(links)
  }, error = function(e) {
    warning("Error extracting links from ", page_url, ": ", e$message)
    return(character())
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
  saveRDS(df, filename)
  message("Saved ", nrow(df), " URLs to ", filename)
}

# Function to check if pagination is complete
is_pagination_complete <- function(pagination_df, last_page) {
  if (is.null(pagination_df)) return(FALSE)
  
  scraped_pages <- unique(pagination_df$page)
  expected_pages <- 1:last_page
  
  missing_pages <- setdiff(expected_pages, scraped_pages)
  
  if (length(missing_pages) > 0) {
    message("Missing pages: ", paste(missing_pages, collapse = ", "))
    return(FALSE)
  }
  
  return(TRUE)
}

# Main SPD scraper function
scrape_spd_press_releases <- function(base_url = party_url, 
                                    force_rescrape = FALSE) {
  # Get last page number first
  last_page <- get_spd_last_page(base_url)
  if (is.null(last_page)) {
    stop("Could not determine last page number")
  }
  
  # Load existing pagination data if any
  pagination_df <- load_pagination_data(party_id)
  
  # Check if pagination is already complete
  if (!force_rescrape && !is.null(pagination_df)) {
    if (is_pagination_complete(pagination_df, last_page)) {
      message("Pagination is already complete for party ID ", party_id)
      return(pagination_df)
    }
  }
  
  # Initialize start page
  start_page <- 1
  if (!is.null(pagination_df)) {
    start_page <- max(pagination_df$page) + 1
    message("Resuming from page ", start_page)
  } else {
    pagination_df <- tibble(
      timestamp = as.POSIXct(character()),
      url = character(),
      page = integer()
    )
  }
  
  message("Total pages to scrape: ", last_page)
  
  # Iterate through pages
  for (page_num in start_page:last_page) {
    message("Processing page ", page_num, " of ", last_page)
    
    # Construct page URL
    if (page_num == 1) {
      page_url <- base_url
    } else {
      page_url <- paste0(base_url, "/page/", page_num)
    }
    
    # Extract press release links
    links <- extract_spd_press_links(page_url)
    
    if (length(links) > 0) {
      new_rows <- tibble(
        timestamp = Sys.time(),
        url = links,
        page = page_num
      )
      
      pagination_df <- bind_rows(pagination_df, new_rows)
      save_pagination_data(pagination_df, party_id)
      Sys.sleep(2)
    }
  }
  
  message("Pagination scraping completed for party ID ", party_id)
  return(pagination_df)
}

# Function to process SPD press releases
process_press_releases_spd <- function(pagination_df) {
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  
  if (file.exists(output_file)) {
    results <- read_csv(output_file)
    message("Loaded ", nrow(results), " existing press releases from CSV")
    remaining_urls <- setdiff(pagination_df$url, results$url)
    pagination_df <- pagination_df[pagination_df$url %in% remaining_urls, ]
    message("Found ", length(remaining_urls), " new URLs to process")
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
      date_collected = as.Date(character())
    )
  }
  
  for (url in pagination_df$url) {
    message("Processing URL: ", url)
    
    tryCatch({
      Sys.sleep(2)
      page <- read_html(url)
      
      title <- page %>%
        html_node(".news__headline") %>%
        html_text() %>%
        str_trim()
      
      date_text <- page %>%
        html_node(".news__kicker") %>%
        html_text() %>%
        str_trim()
      
      date <- str_extract(date_text, "\\d{2}\\.\\d{2}\\.\\d{4}") %>%
        dmy()
      
      text_body <- page %>%
        html_nodes(".text__body")
      
      if (!is.null(text_body)) {
        text_elements <- text_body %>%
          html_nodes("p, blockquote") %>%
          html_text() %>%
          str_trim()
        
        text <- paste(text_elements, collapse = "\n\n")
        text <- str_trim(text)
      } else {
        text <- NA
      }
      
      if (!is.null(title) && !is.na(date)) {
        new_row <- tibble(
          title = title,
          date = date,
          text = text,
          url = url,
          party = "SPD",
          country = country_name,
          year = year(date),
          parlgov_party_id = party_id,
          date_collected = Sys.Date()
        )
        
        results <- bind_rows(results, new_row)
        write_csv(results, output_file)
        message("Saved progress, processed ", nrow(results), " press releases so far")
      }
    }, error = function(e) {
      message("Error processing URL ", url, ": ", e$message)
    })
  }
  
  return(results)
}

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Main execution
message("Starting SPD press release scraper...")

# Get last page number to check against
last_page <- get_spd_last_page(party_url)
if (is.null(last_page)) {
  stop("Could not determine last page number")
}

# Try to load existing data and check if we need to continue scraping
spd_pagination <- tryCatch({
  if (force_rescrape) {
    message("Force rescrape enabled, starting fresh scrape")
    scrape_spd_press_releases(force_rescrape = TRUE)
  } else {
    existing_data <- load_pagination_data(party_id)
    if (!is.null(existing_data)) {
      message("Found existing pagination data")
      if (is_pagination_complete(existing_data, last_page)) {
        message("Pagination is complete, using existing data")
        existing_data
      } else {
        message("Pagination is incomplete, continuing scraping")
        scrape_spd_press_releases(force_rescrape = FALSE)
      }
    } else {
      message("No existing data found, starting fresh scrape")
      scrape_spd_press_releases(force_rescrape = FALSE)
    }
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

# Process press releases
if (!is.null(spd_pagination)) {
  message("Processing press releases...")
  press_releases <- process_press_releases_spd(spd_pagination)
  
  # Final deduplication
  message("Performing final deduplication...")
  initial_rows <- nrow(press_releases)
  
  press_releases <- press_releases %>%
    arrange(url, desc(date_collected)) %>%
    distinct(url, .keep_all = TRUE)
  
  final_rows <- nrow(press_releases)
  
  if (initial_rows != final_rows) {
    message(sprintf("Removed %d duplicate entries", initial_rows - final_rows))
  } else {
    message("No duplicates found")
  }
  
  # Save final results
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  write_csv(press_releases, output_file)
  message(sprintf("Saved %d press releases to %s", nrow(press_releases), output_file))
} else {
  message("No pagination data available, cannot process press releases")
} 
