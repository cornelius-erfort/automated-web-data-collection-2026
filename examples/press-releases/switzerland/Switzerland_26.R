library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

# Set FDP specific variables
base_url <- "https://www.fdp.ch"
party_id <- 26
country_name <- "Switzerland"
party_name <- "FDP"
parlgov_party_id <- 26

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from a page
extract_press_links <- function(page_url) {
  tryCatch({
    page <- read_html(page_url)
    links <- page %>%
      html_nodes(".article .header h2 a") %>%
      html_attr("href") %>%
      unique()
    
    # Prepend base URL to each link
    full_links <- paste0(base_url, links)
    
    return(full_links)
  }, error = function(e) {
    warning("Error extracting links from ", page_url, ": ", e$message)
    return(character())
  })
}

# Function to fetch and save pagination data
fetch_pagination_data <- function(base_url, archive_links) {
  all_links <- character()
  
  for (archive_link in archive_links) {
    next_page_url <- paste0(base_url, archive_link)
    
    while (!is.null(next_page_url)) {
      message("Processing page: ", next_page_url)
      
      page <- read_html(next_page_url)
      links <- extract_press_links(next_page_url)
      all_links <- c(all_links, links)
      
      # Find the next page link
      next_page_node <- page %>% html_node(".pagination .next a")
      next_page_url <- if (!is.null(next_page_node) & !is.na(next_page_node)) {
        paste0(base_url, next_page_node %>% html_attr("href"))
      } else {
        NULL
      }
      
      Sys.sleep(2)
    }
  }
  
  return(all_links)
}

# Function to extract archive links with hashes
extract_archive_links <- function(base_url) {
  archive_url <- paste0(base_url, "/aktuell/medienmitteilungen")
  page <- read_html(archive_url)
  
  archive_links <- page %>%
    html_nodes(".news-menu-view a") %>%
    html_attr("href") %>%
    unique()
  
  return(archive_links)
}

# Function to translate Swiss dates to English format
parse_swiss_date <- function(swiss_date) {
  month_dict <- c(
    "Januar" = "January", "Februar" = "February", "März" = "March", 
    "April" = "April", "Mai" = "May", "Juni" = "June", 
    "Juli" = "July", "August" = "August", "September" = "September", 
    "Oktober" = "October", "November" = "November", "Dezember" = "December"
  )
  
  for (german_month in names(month_dict)) {
    english_month <- month_dict[[german_month]]
    swiss_date <- str_replace(swiss_date, german_month, english_month)
  }
  
  parsed_date <- dmy(swiss_date)
  return(parsed_date)
}

# Function to scrape detailed content from individual pages
scrape_detailed_content <- function(url) {
  tryCatch({
    # Ensure the URL is complete
    full_url <- ifelse(startsWith(url, "http"), url, paste0(base_url, url))
    page <- read_html(full_url)
    title <- page %>% html_node("h1[itemprop='headline']") %>% html_text(trim = TRUE)
    date_french <- page %>% html_node(".news-list-date time") %>% html_text(trim = TRUE)
    date <- parse_swiss_date(date_french)
    content <- page %>% html_node(".news-text-wrap") %>% html_text(trim = TRUE)
    list(title = title, date = date, content = content)
  }, error = function(e) {
    message("Error scraping detailed content from ", url, ": ", e$message)
    return(list(title = NA, date = NA, content = NA))
  })
}

# Main execution
message("Starting FDP press release scraper...")

# Fetch archive links with hashes
archive_links <- extract_archive_links(base_url)
message("Archive links: ", paste(archive_links, collapse = ", "))

# Prepare CSV file
output_file <- file.path("press_release_data", paste0(tolower(country_name), "_", party_id, ".csv"))
results <- tibble(
  title = character(),
  date = as.Date(character()),
  content = character(),
  url = character(),
  party = character(),
  country = character(),
  year = integer(),
  parlgov_party_id = integer(),
  date_collected = as.Date(character())
)

# Fetch current year pagination data
current_links <- fetch_pagination_data(base_url, c("/aktuell/medienmitteilungen"))

# Fetch archive year pagination data
all_links <- fetch_pagination_data(base_url, archive_links)

# Combine all links
all_links <- c(current_links, all_links)

for (i in seq_along(all_links)) {
  url <- all_links[i]
  message("Processing URL: ", url)
  
  detailed_content <- scrape_detailed_content(url)
  
  # Add detailed content to results
  new_row <- tibble(
    title = detailed_content$title,
    date = detailed_content$date,
    content = detailed_content$content,
    url = url,
    party = party_name,
    country = country_name,
    year = year(detailed_content$date),
    parlgov_party_id = parlgov_party_id,
    date_collected = Sys.Date()
  )
  
  results <- bind_rows(results, new_row)
  
  # Save updated data to CSV
  write_csv(results, output_file)
  message("Updated and saved press release ", i, " to CSV")
}

message("Scraping completed.")
