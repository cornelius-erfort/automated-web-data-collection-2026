library(tidyverse)
library(httr)
library(rvest)
library(lubridate)
library(jsonlite)
library(here)

# Set KESK specific variables
party_url <- "https://keskusta.fi/wp-admin/admin-ajax.php"
party_id <- 94
country_name <- "Finland"

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links using AJAX
extract_kesk_press_links <- function() {
  tryCatch({
    # Make POST request to get all news items
    response <- POST(
      url = party_url,
      body = list(
        action = "moreNews",
        index = 1,
        amount = 10000,
        category = 12
      ),
      encode = "form"
    )
    
    # Parse JSON response
    json_data <- fromJSON(rawToChar(response$content))
    
    # Extract links from HTML items
    links <- sapply(json_data$items, function(html_str) {
      parsed <- read_html(html_str)
      href <- parsed %>%
        html_node("h3 a") %>%
        html_attr("href")
      return(href)
    })
    
    # Return unique, non-NA links
    unique(links[!is.na(links)])
    
  }, error = function(e) {
    warning(sprintf("Error extracting links: %s", e$message))
    return(character(0))
  })
}

# Function to process a single press release
process_kesk_press_release <- function(url) {
  tryCatch({
    # Read the page
    page <- read_html(url)
    
    # Extract title from meta tags
    title <- page %>%
      html_node("title") %>%
      html_text() %>%
      str_remove(" - Suomen Keskusta$") %>%
      trimws()
    
    # Extract date from meta tags
    date <- page %>%
      html_node("meta[property='article:published_time']") %>%
      html_attr("content") %>%
      as.POSIXct(format = "%Y-%m-%dT%H:%M:%S%z") %>%
      as.Date()
    
    # If meta date is not available, try visible date
    if(is.na(date)) {
      date_text <- page %>%
        html_node(".post-date") %>%
        html_text() %>%
        str_extract("\\d{1,2}\\.\\d{1,2}\\.\\d{4}")
      
      if(!is.na(date_text)) {
        parts <- str_split(date_text, "\\.")[[1]]
        date <- as.Date(sprintf("%s-%s-%s", parts[3], parts[2], parts[1]))
      }
    }
    
    # Extract text content
    text_parts <- page %>%
      html_nodes(".col-text p") %>%
      html_text() %>%
      trimws() %>%
      .[nchar(.) > 0] %>%  # Remove empty paragraphs
      .[!str_detect(., "^Jaa artikkeli:")] # Remove share text
    
    # Combine text parts
    text <- paste(text_parts, collapse = "\n\n")
    
    # Return as tibble row
    if(!is.na(title) && !is.na(date) && !is.na(text)) {
      tibble(
        title = title,
        date = date,
        text = text,
        url = url,
        party = "KESK",
        country = "Finland",
        year = year(date),
        parlgov_party_id = 94,
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
extract_kesk_press_releases <- function() {
  # Set up output file path
  output_file <- file.path("press_release_data", "finland_94.csv")
  
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
  links <- extract_kesk_press_links()
  message(sprintf("Found %d total links", length(links)))
  
  # Process new links
  new_links <- setdiff(links, existing_urls)
  if(length(new_links) > 0) {
    message(sprintf("Processing %d new links", length(new_links)))
    for(url in new_links) {
      message(sprintf("Processing press release: %s", url))
      result <- process_kesk_press_release(url)
      
      if(!is.null(result)) {
        # Append to CSV
        write_csv(result, output_file, append = TRUE)
        message(sprintf("Saved press release: %s", result$title))
      }
      
      Sys.sleep(1)  # Rate limiting
    }
  }
  
  # Read and return final results
  final_results <- read_csv(output_file, show_col_types = FALSE)
  message(sprintf("Total press releases in file: %d", nrow(final_results)))
  
  return(final_results)
}

# Run the scraper
press_releases <- extract_kesk_press_releases() 