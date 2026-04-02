library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

# Set Rassemblement National specific variables
base_url <- "https://rassemblementnational.fr/communiques?page="
party_id <- 270
country_name <- "France"
party_name <- "Rassemblement National"
parlgov_party_id <- 270

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from a page
extract_press_links <- function(page_url) {
  tryCatch({
    page <- read_html(page_url)
    links <- page %>%
      html_nodes("a[href*='/communiques/']") %>%
      html_attr("href") %>%
      unique()
    
    # Correct malformed URLs
    links <- sapply(links, function(link) {
      if (!startsWith(link, "https://")) {
        link <- paste0("https://rassemblementnational.fr", link)
      }
      link <- gsub("https//", "https://", link)  # Fix malformed URLs
      return(link)
    })
    
    return(links)
  }, error = function(e) {
    warning("Error extracting links from ", page_url, ": ", e$message)
    return(character())
  })
}

# Function to translate French dates to English format
translate_french_date <- function(french_date) {
  month_dict <- c(
    "janvier" = "January", "février" = "February", "mars" = "March", 
    "avril" = "April", "mai" = "May", "juin" = "June", 
    "juillet" = "July", "août" = "August", "septembre" = "September", 
    "octobre" = "October", "novembre" = "November", "décembre" = "December"
  )
  
  # Replace French month with English month
  for (french_month in names(month_dict)) {
    english_month <- month_dict[[french_month]]
    french_date <- str_replace(french_date, french_month, english_month)
  }
  
  return(french_date)
}

# Function to scrape detailed content from individual pages
scrape_detailed_content <- function(url) {
  tryCatch({
    page <- read_html(url)
    title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
    (date_french <- page %>% html_node(".items-start p.text-gray-500") %>% html_text(trim = TRUE))
    date_english <- translate_french_date(date_french)
    date <- dmy(date_english)
    content <- page %>% html_node("div.text-justify") %>% html_text(trim = TRUE)
    list(title = title, date = date, content = content)
  }, error = function(e) {
    message("Error scraping detailed content from ", url, ": ", e$message)
    return(list(title = NA, date = NA, content = NA))
  })
}

# Main execution
message("Starting Rassemblement National press release scraper...")

# Load existing pagination data if available
rds_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
if (file.exists(rds_file)) {
  message("Loading existing pagination data from ", rds_file)
  pagination_data <- readRDS(rds_file)
} else {
  pagination_data <- tibble(url = character())
}

# Prepare CSV file
output_file <- file.path("press_release_data", paste0(tolower(country_name), "_", party_id, ".csv"))
if (file.exists(output_file)) {
  message("Loading existing press releases from CSV")
  results <- read_csv(output_file)
} else {
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
}

# Fetch and process each page
page_num <- 1
consecutive_empty_pages <- 0
max_empty_pages <- 3  # Stop after 3 consecutive pages with no links

while (consecutive_empty_pages < max_empty_pages) {
  page_url <- paste0(base_url, page_num)
  message("Processing page ", page_num)
  
  links <- extract_press_links(page_url)
  
  if (length(links) > 0) {
    consecutive_empty_pages <- 0  # Reset counter when links are found
    new_links <- setdiff(links, pagination_data$url)
    
    if (length(new_links) > 0) {
      pagination_data <- bind_rows(pagination_data, tibble(url = new_links))
      saveRDS(pagination_data, rds_file)
      
      for (url in new_links) {
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
        message("Updated and saved press release to CSV")
      }
    }
  } else {
    consecutive_empty_pages <- consecutive_empty_pages + 1
    message("No links found on page ", page_num, 
            ". Empty pages count: ", consecutive_empty_pages)
  }
  
  Sys.sleep(2)
  page_num <- page_num + 1
}

# Save pagination data to RDS
saveRDS(pagination_data, rds_file)
message("Saved pagination data to ", rds_file) 