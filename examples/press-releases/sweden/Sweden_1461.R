library(httr)
library(jsonlite)
library(tidyverse)
library(rvest)

# Set Center Party specific variables
party_url <- "https://www.centerpartiet.se/press/nyheter"
api_url <- "https://www.centerpartiet.se/rest-api/article-service?start=0&num=10000&path=4.7178c78e15577d7eba42ae&startDate=&endDate="
party_id <- 1461
country_name <- "Sweden"
party_name <- "Centerpartiet"
parlgov_party_id <- 1461

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Mapping of Swedish month names to English
swedish_to_english_months <- c(
  "januari" = "January",
  "februari" = "February",
  "mars" = "March",
  "april" = "April",
  "maj" = "May",
  "juni" = "June",
  "juli" = "July",
  "augusti" = "August",
  "september" = "September",
  "oktober" = "October",
  "okt" = "October",
  "november" = "November",
  "december" = "December"
)

# Function to translate and parse Swedish dates
parse_swedish_date <- function(date_string) {
  # Replace Swedish month names with English
  for (swedish_month in names(swedish_to_english_months)) {
    date_string <- str_replace(date_string, swedish_month, swedish_to_english_months[swedish_month])
  }
  
  # Parse the date
  as.POSIXct(date_string, format = "%d %B %Y %H:%M", tz = "CET") %>% as.Date
}

# Function to fetch and save pagination data
fetch_and_save_pagination <- function(api_url) {
  tryCatch({
    response <- GET(api_url)
    if (status_code(response) != 200) {
      stop("Failed to fetch data, status code: ", status_code(response))
    }
    
    data <- fromJSON(rawToChar(response$content))
    hits <- data$hits
    
    pagination_df <- tibble(
      date = sapply(hits, function(x) x$date),
      title = sapply(hits, function(x) x$title),
      uri = sapply(hits, function(x) x$uri),
      url = paste0("https://www.centerpartiet.se", sapply(hits, function(x) x$uri))
    )
    
    # Save pagination data
    pagination_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
    saveRDS(pagination_df, pagination_file)
    message("Saved pagination data with ", nrow(pagination_df), " entries to ", pagination_file)
    
    return(pagination_df)
    
  }, error = function(e) {
    message("Error fetching pagination data: ", e$message)
    return(NULL)
  })
}

# Function to fetch and parse individual press release pages
fetch_press_release_details <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Extract the date
    date_text <- page %>%
      html_node(".sol-article-date") %>%
      html_text(trim = TRUE) %>%
      str_extract("\\d{1,2} \\w+ \\d{4} \\d{2}:\\d{2}")
    
    # Translate and parse the date
    date <- parse_swedish_date(date_text)
    
    # Extract the title
    title <- page %>%
      html_node("h1.font-heading-1") %>%
      html_text(trim = TRUE)
    
    # Extract the main content
    content <- page %>%
      html_nodes(".sv-text-portlet-content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    # Return a tibble with the extracted data
    return(tibble(
      date = date,
      title = title,
      content = content,
      url = url,
      party = party_name,
      country = country_name,
      year = if (!is.na(date)) year(date) else NA_integer_,
      parlgov_party_id = parlgov_party_id,
      date_collected = Sys.Date()
    ))
    
  }, error = function(e) {
    message("Error fetching details for URL ", url, ": ", e$message)
    return(NULL)
  })
}

# Main execution
message("Starting Centerpartiet press release scraper...")

# Step 1: Fetch and save pagination data
# pagination_df <- fetch_and_save_pagination(api_url)
pagination_df <- readRDS(file.path("meta-data", paste0("pagination_", party_id, ".rds")))

# Step 2: Load existing detailed data if available
output_file <- file.path("press_release_data", paste0(tolower(country_name), "_", party_id, ".csv"))
if (file.exists(output_file)) {
  detailed_press_releases <- read_csv(output_file)
  message("Loaded existing detailed data with ", nrow(detailed_press_releases), " entries")
} else {
  detailed_press_releases <- tibble()
}

# Step 3: Process each press release
if (!is.null(pagination_df)) {
  for (i in seq_len(nrow(pagination_df))) {
    url <- pagination_df$url[i]
    
    # Skip URLs that have already been processed
    if (url %in% detailed_press_releases$url) {
      message(sprintf("Skipping already processed URL %d of %d: %s", i, nrow(pagination_df), url))
      next
    }
    
    message(sprintf("Processing %d of %d: %s", i, nrow(pagination_df), url))
    
    details <- fetch_press_release_details(url)
    if (!is.null(details)) {
      detailed_press_releases <- bind_rows(detailed_press_releases, details)
      
      # Output a preview of the data
      message("Preview of extracted data:")
      print(details)
      
      # Save the updated data to the CSV file
      write_csv(detailed_press_releases, output_file)
      message("Saved updated detailed data to ", output_file)
    }
  }
} else {
  message("No pagination data available, cannot process press releases")
} 
