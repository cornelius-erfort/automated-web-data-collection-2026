library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

# Set SVP specific variables
base_url <- "https://www.svp.ch/publikationen/medienmitteilungen/"
party_id <- 750
country_name <- "Switzerland"
party_name <- "SVP"
parlgov_party_id <- 750

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from a page
extract_press_links <- function(page_url) {
  tryCatch({
    page <- read_html(page_url)
    links <- page %>%
      html_nodes(".postdetailliste li a") %>%
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
  all_links <- character()

    links <- extract_press_links(base_url)
    
    if (length(links) > 0) {
      consecutive_empty_pages <- 0  # Reset counter when links are found
      all_links <- c(all_links, links)
    } else {
      consecutive_empty_pages <- consecutive_empty_pages + 1
      message("No links found on page ", page_num, 
              ". Empty pages count: ", consecutive_empty_pages)
    }
    

  # Save pagination data to RDS
  pagination_data <- tibble(url = all_links)
  rds_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  saveRDS(pagination_data, rds_file)
  
  message("Saved pagination data to ", rds_file)
  
  return(pagination_data)
}

# Function to translate and parse Swiss dates
parse_swiss_date <- function(swiss_date) {
  # Dictionary to translate German month names to English
  month_dict <- c(
    "Januar" = "January", "Februar" = "February", "März" = "March", 
    "April" = "April", "Mai" = "May", "Juni" = "June", 
    "Juli" = "July", "August" = "August", "September" = "September", 
    "Oktober" = "October", "November" = "November", "Dezember" = "December"
  )
  
  # Replace German month with English month
  for (german_month in names(month_dict)) {
    english_month <- month_dict[[german_month]]
    swiss_date <- str_replace(swiss_date, german_month, english_month)
  }
  
  # Parse the date
  parsed_date <- dmy(swiss_date)
  
  return(parsed_date)
}

# Function to scrape detailed content from individual pages
scrape_detailed_content <- function(url) {
  tryCatch({
    
    # Read the HTML content from the URL
    html_content <- read_html(url)
    
    # Remove all @ signs from the HTML content
    clean_html_content <- str_replace_all(as.character(html_content), "@", "")
    
    # Parse the cleaned HTML content
    page <- read_html(clean_html_content)
    
    # Extract the title
    title <- page %>% html_node("h1.uk-article-title") %>% html_text(trim = TRUE)
    
    # Extract and parse the date
    date_french <- page %>% html_node(".post-date") %>% html_text(trim = TRUE)
    date <- parse_swiss_date(date_french)
    
    # page <- read_html(url)
    # title <- page %>% html_node("h1.uk-article-title") %>% html_text(trim = TRUE)
    # date_french <- page %>% html_nodes(".post-date") %>% html_text(trim = TRUE)
    # date <- parse_swiss_date(date_french)
    # ".uk-text-large, "
    
    # Updated CSS selector for the content
    content <- page %>% html_nodes(".uk-width-2-3m > .uk-text-large, .siteorigin-widget-tinymce") %>% html_text(trim = TRUE)
    
    # Combine all content into a single string
    full_content <- paste(content, collapse = " ")
    
    # content <- page %>% html_nodes(".singleContent  div:nth-child(2)")
    # content <- content[length(content)] %>% html_text(trim = TRUE)
    list(title = title, date = date, content = full_content)
  }, error = function(e) {
    message("Error scraping detailed content from ", url, ": ", e$message)
    return(list(title = NA, date = NA, content = NA))
  })
}

# Main execution
message("Starting SVP press release scraper...")
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
