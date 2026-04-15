library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(here)

# Set KOK specific variables
party_url <- "https://www.sttinfo.fi/public-website-api/pressroom/69819274/releases/2000/0"
release_base_url <- "https://www.sttinfo.fi/public-website-api/release/"
party_id <- 1118
country_name <- "Finland"

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from the API
extract_kok_press_links <- function() {
  tryCatch({
    # Get the JSON response
    response <- GET(party_url)
    
    if (status_code(response) != 200) {
      warning(sprintf("Failed to get press releases list. Status code: %d", status_code(response)))
      return(character(0))
    }
    
    # Parse JSON
    data <- fromJSON(rawToChar(response$content))
    
    # Extract release IDs and dates
    releases <- data$releases %>%
      select(id, date) %>%
      mutate(
        url = paste0(release_base_url, id),
        date = as.Date(date)
      )
    
    return(releases)
    
  }, error = function(e) {
    warning(sprintf("Error extracting links: %s", e$message))
    return(tibble())
  })
}

# Function to process a single press release
process_kok_press_release <- function(url) {
  tryCatch({
    # Get the JSON response
    response <- GET(url)
    
    if (status_code(response) != 200) {
      warning(sprintf("Failed to get press release. Status code: %d", status_code(response)))
      return(NULL)
    }
    
    # Parse JSON
    data <- fromJSON(rawToChar(response$content))
    
    # Extract Finnish version content
    fi_version <- data$versions$fi
    
    # Extract fields
    title <- fi_version$title
    date <- as.Date(data$date)
    
    # Combine metadescription and body text
    text_parts <- c(
      fi_version$metadescription,
      fi_version$body$complete %>% 
        gsub("<[^>]+>", "", .) %>%  # Remove HTML tags
        gsub("\n+", "\n", .) %>%    # Normalize newlines
        trimws()
    )
    
    text <- paste(text_parts[!is.na(text_parts)], collapse = "\n\n")
    
    # Return as tibble row
    if(!is.na(title) && !is.na(date) && !is.na(text)) {
      tibble(
        title = title,
        date = date,
        text = text,
        url = url,
        party = "KOK",
        country = "Finland",
        year = year(date),
        parlgov_party_id = 1118,
        date_collected = Sys.Date()
      )
    } else {
      NULL
    }
    
  }, error = function(e) {
    warning(sprintf("Error processing %s: %s", url, e$message))
    return(NULL)
  })
}

# Main function to extract and process press releases
extract_kok_press_releases <- function() {
  # Set up output file paths
  output_file <- file.path("press_release_data", "finland_1118.csv")
  pagination_file <- file.path("meta-data", sprintf("pagination_%d.rds", party_id))
  
  # Create empty CSV with headers if it doesn't exist
  if (!file.exists(output_file)) {
    tibble(
      title = character(),
      date = as.Date(character()),
      text = character(),
      url = character(),
      party = character(),
      country = character(),
      year = integer(),
      parlgov_party_id = integer(),
      date_collected = as.Date(character())
    ) %>% write_csv(output_file)
  }
  
  # Read existing URLs to avoid duplicates
  existing_data <- if(file.exists(output_file)) {
    read_csv(output_file, show_col_types = FALSE)
  } else {
    tibble()
  }
  existing_urls <- existing_data$url
  
  # Get all press release links
  releases <- extract_kok_press_links()
  message(sprintf("Found %d press releases", nrow(releases)))
  
  # Filter out already processed URLs
  new_releases <- releases %>%
    filter(!url %in% existing_urls)
  message(sprintf("Found %d new press releases to process", nrow(new_releases)))
  
  # Process each press release
  for(i in 1:nrow(new_releases)) {
    url <- new_releases$url[i]
    message(sprintf("Processing press release %d/%d: %s", 
                   i, nrow(new_releases), url))
    
    result <- process_kok_press_release(url)
    
    if(!is.null(result)) {
      # Append to CSV
      write_csv(result, output_file, append = TRUE)
      message(sprintf("Saved press release: %s", result$title))
    }
    
    Sys.sleep(1)  # Rate limiting
  }
  
  # Read and return final results
  final_results <- read_csv(output_file, show_col_types = FALSE)
  message(sprintf("Total press releases in file: %d", nrow(final_results)))
  
  return(final_results)
}

# Run the scraper
press_releases <- extract_kok_press_releases() 
