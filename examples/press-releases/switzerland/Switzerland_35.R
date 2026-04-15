library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

# Set SP-PS specific variables
base_url <- "https://www.sp-ps.ch/artikel-kanal/medienmitteilungen/?jsf=epro-archive&pagenum="
party_id <- 35
country_name <- "Switzerland"
party_name <- "SP-PS"
parlgov_party_id <- 35

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from a page
extract_press_links <- function(page_url) {
  tryCatch({
    page <- read_html(page_url)
    links <- page %>%
      html_nodes(".elementor-post__title a") %>%
      html_attr("href") %>%
      unique()
    
    return(links)
  }, error = function(e) {
    warning("Error extracting links from ", page_url, ": ", e$message)
    return(character())
  })
}

# Function to fetch and save pagination data
fetch_pagination_data <- function(base_url) {
  page_num <- 1
  all_links <- character()
  consecutive_empty_pages <- 0
  max_empty_pages <- 3  # Stop after 3 consecutive pages with no links
  
  while (consecutive_empty_pages < max_empty_pages) {
    page_url <- paste0(base_url, page_num)
    message("Processing page ", page_num)
    
    links <- extract_press_links(page_url)
    
    if (length(links) > 0) {
      consecutive_empty_pages <- 0  # Reset counter when links are found
      all_links <- c(all_links, links)
    } else {
      consecutive_empty_pages <- consecutive_empty_pages + 1
      message("No links found on page ", page_num, 
              ". Empty pages count: ", consecutive_empty_pages)
    }
    
    Sys.sleep(2)
    page_num <- page_num + 1
  }
  
  # Save pagination data to RDS
  pagination_data <- tibble(url = all_links)
  rds_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  saveRDS(pagination_data, rds_file)
  
  message("Saved pagination data to ", rds_file)
  
  return(pagination_data)
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
    page <- read_html(url)
    title <- page %>% html_node("h1.elementor-heading-title") %>% html_text(trim = TRUE)
    date_french <- page %>% html_node("time") %>% html_text(trim = TRUE)
    date <- parse_swiss_date(date_french)
    content <- page %>% html_node(".elementor-widget-text-editor") %>% html_text(trim = TRUE)
    list(title = title, date = date, content = content)
  }, error = function(e) {
    message("Error scraping detailed content from ", url, ": ", e$message)
    return(list(title = NA, date = NA, content = NA))
  })
}

# Main execution
message("Starting SP-PS press release scraper...")
pagination_data <- fetch_pagination_data(base_url)

if (!is.null(pagination_data)) {
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
  
  for (i in seq_len(nrow(pagination_data))) {
    url <- pagination_data$url[i]
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
}
