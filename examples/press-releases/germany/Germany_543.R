library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)
library(xml2)
library(jsonlite)
library(here)

# Set FDP specific variables
party_url <- "https://www.fdp.de/uebersicht/pressemitteilungen?search_terms=&sort=created_desc&language=de&type=press_release&page=0"
party_id <- 543
country_name <- "Germany"
force_rescrape <- FALSE

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

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

# Function to extract press release links from a page
extract_fdp_press_links <- function(page_url) {
  tryCatch({
    page <- read_html(page_url)
    links <- page %>%
      html_nodes(".ntype-press-release") %>%
      html_attr("href") %>%
      unique()
    
    # Convert relative URLs to absolute
    links <- paste0("https://www.fdp.de", links)
    
    return(links)
  }, error = function(e) {
    warning("Error extracting links from ", page_url, ": ", e$message)
    return(character())
  })
}

# Main FDP scraper function
scrape_fdp_press_releases <- function(base_url = party_url, 
                                    force_rescrape = FALSE) {
  # Load existing pagination data if any
  pagination_df <- load_pagination_data(party_id)
  
  # Initialize start page
  start_page <- 0
  if (!is.null(pagination_df) && !force_rescrape) {
    start_page <- max(pagination_df$page) + 1
    message("Resuming from page ", start_page)
  } else {
    pagination_df <- tibble(
      timestamp = as.POSIXct(character()),
      url = character(),
      page = integer()
    )
  }
  
  # Initialize variables
  page_num <- start_page
  consecutive_empty_pages <- 0
  max_empty_pages <- 3  # Stop after 3 consecutive pages with no links
  
  while (consecutive_empty_pages < max_empty_pages) {
    message("Processing page ", page_num)
    
    # Construct page URL
    page_url <- str_replace(base_url, "page=0", paste0("page=", page_num))
    
    # Extract press release links
    links <- extract_fdp_press_links(page_url)
    
    if (length(links) > 0) {
      consecutive_empty_pages <- 0  # Reset counter when links are found
      
      new_rows <- tibble(
        timestamp = Sys.time(),
        url = links,
        page = page_num
      )
      
      pagination_df <- bind_rows(pagination_df, new_rows)
      save_pagination_data(pagination_df, party_id)
      
    } else {
      consecutive_empty_pages <- consecutive_empty_pages + 1
      message("No links found on page ", page_num, 
              ". Empty pages count: ", consecutive_empty_pages)
    }
    
    Sys.sleep(2)
    page_num <- page_num + 1
  }
  
  message("Pagination scraping completed for party ID ", party_id)
  return(pagination_df)
}

# Function to process FDP press releases
process_press_releases_fdp <- function(pagination_df) {
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
      date_collected = as.Date(character())
    )
  }
  
  remaining_urls <- setdiff(pagination_df$url, results$url)
  pagination_df <- pagination_df[pagination_df$url %in% remaining_urls, ]
  total_remaining <- length(remaining_urls)
  message("Processing ", total_remaining, " remaining press releases...")
  
  for (url in remaining_urls) {
    message("Processing URL: ", url)
    
    tryCatch({
      Sys.sleep(2)
      page <- read_html(url)
      
      title <- page %>%
        html_node("h1") %>%
        html_text() %>%
        str_trim()
      
      date <- page %>%
        html_node(".meta-data time") %>%
        html_text() %>%
        dmy()
      
      text_elements <- page %>%
        html_nodes(".paragraph.type-text p") %>%
        html_text() %>%
        str_trim()
      
      text <- paste(text_elements, collapse = "\n\n")
      
      if (!is.null(title) && !is.na(date)) {
        new_row <- tibble(
          title = title,
          date = date,
          text = text,
          url = url,
          party = "FDP",
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

# Main execution
message("Starting FDP press release scraper...")

# Modified execution logic to use force_rescrape parameter
fdp_pagination <- if (force_rescrape) {
  message("Force rescrape enabled, starting fresh scrape")
  scrape_fdp_press_releases(force_rescrape = TRUE)
} else {
  tryCatch({
    existing_data <- load_pagination_data(party_id)
    if (!is.null(existing_data)) {
      message("Found existing pagination data, continuing from last page")
      scrape_fdp_press_releases(force_rescrape = FALSE)
    } else {
      message("No existing data found, starting fresh scrape")
      scrape_fdp_press_releases(force_rescrape = TRUE)
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

if (!is.null(fdp_pagination)) {
  message("Processing press releases...")
  press_releases <- process_press_releases_fdp(fdp_pagination)
  
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