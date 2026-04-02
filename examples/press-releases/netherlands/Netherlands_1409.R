library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)
library(xml2)
library(jsonlite)
library(here)

# Set VVD specific variables
party_url <- "https://www.vvd.nl/nieuws/"
party_id <- 1409
country_name <- "Netherlands"

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Dutch month conversion lookup
dutch_months <- c(
  "januari" = "01", "februari" = "02", "maart" = "03", "april" = "04",
  "mei" = "05", "juni" = "06", "juli" = "07", "augustus" = "08",
  "september" = "09", "oktober" = "10", "november" = "11", "december" = "12"
)

# Function to convert Dutch date to ISO format
convert_dutch_date <- function(date_str) {
  tryCatch({
    # Extract components using regex
    if (is.null(date_str) || is.na(date_str)) return(NA)
    
    parts <- str_match(date_str, "([0-9]{1,2})\\s+([a-z]+)\\s+([0-9]{4})")
    if (is.na(parts[1])) return(NA)
    
    day <- sprintf("%02d", as.numeric(parts[2]))
    month <- dutch_months[tolower(parts[3])]
    year <- parts[4]
    
    # Combine into ISO format
    date <- as.Date(paste(year, month, day, sep = "-"))
    return(date)
  }, error = function(e) {
    warning("Error converting date: ", date_str, " - ", e$message)
    return(NA)
  })
}

# Function to extract press release links from a page
extract_vvd_press_links <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Get all news items
    news_items <- page %>% 
      html_nodes(".news--item")
    
    if(length(news_items) == 0) {
      news_items <- page %>% 
        html_nodes(".featured-news--item")
    }
    
    # Initialize empty vectors for collecting data
    links <- character()
    
    for (item in news_items) {
      # Get the link
      link <- item %>% 
        html_attr("href")

      
      if (!is.na(link)) {
        links <- c(links, link)
      }
    }
    
    # Create a dataframe with all the information
    results <- tibble(
      url = links
    )
    
    return(results)
    
  }, error = function(e) {
    warning("Error extracting links from ", url, ": ", e$message)
    return(tibble(
      url = character()
    ))
  })
}

# Function to process a single press release
process_vvd_press_release <- function(url) {
  tryCatch({
    Sys.sleep(1)  # Rate limiting
    page <- read_html(url)
    
    # Extract title
    title <- page %>%
      html_node("h1") %>%
      html_text(trim = TRUE)
    
    # Extract date - adjust selectors based on VVD's HTML structure
    date_str <- page %>%
      html_node(".news-single-banner--date") %>%  # Adjust this selector
      html_text(trim = TRUE)
    
    date <- convert_dutch_date(date_str)   # Adjust date parsing if needed
    
    # Extract text content
    text <- page %>%
      html_nodes(".content p") %>%  # Adjust this selector
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    # Return as tibble row
    tibble(
      title = title,
      date = date,
      text = text,
      url = url
    )
    
  }, error = function(e) {
    warning("Error processing URL ", url, ": ", e$message)
    return(NULL)
  })
}

# Function to get total number of pages
get_total_pages <- function(url) {
    page <- read_html(url)
    
    # Find the last page number from pagination
    last_page <- page %>%
      html_nodes(".pagination li:nth-last-child(1) a") %>%
      html_text(trim = TRUE) %>% str_extract("\\d{1,3}") %>% 
      as.numeric()
    
    return(last_page)
    
  }, error = function(e) {
    warning("Error getting total pages: ", e$message)
    return(NULL)
  })
}

# Phase 1: Gather all links through pagination
gather_press_release_links <- function() {
  # Define pagination file path
  pagination_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  
  # Try to load existing pagination data
  if (file.exists(pagination_file)) {
    message("Loading existing pagination data...")
    all_links <- readRDS(pagination_file)
    last_page <- max(all_links$page_num)
    message(sprintf("Found existing data with %d links, last page: %d", 
                   nrow(all_links), last_page))
    start_page <- last_page + 1
  } else {
    message("No existing pagination data found. Starting fresh...")
    all_links <- tibble(
      url = character()
    )
    start_page <- 1
  }
  
  # Get total number of pages
  total_pages <- get_total_pages(party_url)
  if (is.null(total_pages)) {
    message("Could not determine total pages, using default of 28")
    total_pages <- 28
  }
  
  # Process each page
  for (page_num in start_page:total_pages) {
    message(sprintf("Processing page %d of %d", page_num, total_pages))
    
    # Construct page URL
    if (page_num == 1) {
      page_url <- party_url
    } else {
      page_url <- paste0(party_url, "page/", page_num, "/")
    }
    
    # Extract press releases from this page
    page_releases <- extract_vvd_press_links(page_url)
    
    if (nrow(page_releases) > 0) {
      # Convert dates
      page_releases <- page_releases %>%
        mutate(
          page_num = page_num,
          timestamp = Sys.time()
        )
      
      # Do not add duplicates
      page_releases <- page_releases[!(page_releases$url %in% all_links$url), ]
      
      # Add to our collection of links
      all_links <- bind_rows(all_links, page_releases)
      message(sprintf("Found %d links on page %d. Total links: %d", 
                     nrow(page_releases), page_num, nrow(all_links)))
      
      # Save progress after each page
      saveRDS(all_links, pagination_file)
      message(sprintf("Updated pagination file with %d total links", nrow(all_links)))
    } else {
      message(sprintf("No links found on page %d", page_num))
    }
    
    # Rate limiting between pages
    Sys.sleep(2)
  }
  
  return(all_links)
}

# Phase 2: Process press releases
process_press_releases <- function() {
  # Load pagination data
  pagination_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  if (!file.exists(pagination_file)) {
    stop("No pagination data found. Please run gather_press_release_links first.")
  }
  
  all_links <- readRDS(pagination_file)
  message(sprintf("Loaded %d links from pagination file", nrow(all_links)))
  
  # Set up output file
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  
  # Initialize or load existing results
  if (file.exists(output_file)) {
    message("Loading existing press release data...")
    results <- read_csv(output_file)
    message(sprintf("Loaded %d existing press releases", nrow(results)))
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
    write_csv(results, output_file)  # Create the file
  }
  
  # Process each link
  for (i in 1:nrow(all_links)) {
    url <- all_links$url[i]
    
    # Check if URL already processed
    if (url %in% results$url) {
      message(sprintf("Skipping already processed URL (%d/%d): %s", 
                     i, nrow(all_links), url))
      next
    }
    
    message(sprintf("Processing release (%d/%d): %s", 
                   i, nrow(all_links), url))
    
    release <- process_vvd_press_release(url)
    
    if (!is.null(release)) {
      # Add party specific information
      release <- release %>%
        mutate(
          party = "VVD",
          country = country_name,
          year = year(date),
          parlgov_party_id = party_id,
          date_collected = Sys.Date()
        )
      
      # Append to file directly
      write_csv(release, output_file, append = TRUE)
      message("Saved press release to file")
    }
    
    # Rate limiting between requests
    Sys.sleep(1)
  }
  
  # Return final results
  message("Reading final results...")
  final_results <- read_csv(output_file)
  return(final_results)
}

# Main execution function
scrape_vvd_press_releases <- function(phase = c("both", "links", "process")) {
  phase <- match.arg(phase)
  
  if (phase %in% c("both", "links")) {
    message("Starting Phase 1: Gathering press release links...")
    links <- gather_press_release_links()
    message(sprintf("Completed Phase 1: Gathered %d links", nrow(links)))
  }
  
  if (phase %in% c("both", "process")) {
    message("Starting Phase 2: Processing press releases...")
    results <- process_press_releases()
    message(sprintf("Completed Phase 2: Processed %d press releases", nrow(results)))
    return(results)
  }
  
  if (phase == "links") {
    return(links)
  }
}

# Execute scraping
message("Starting VVD press release scraper...")
press_releases <- scrape_vvd_press_releases(phase = "process") 

