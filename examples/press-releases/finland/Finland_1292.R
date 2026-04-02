library(tidyverse)
library(httr)
library(rvest)
library(lubridate)
library(jsonlite)
library(here)

# Set DLVAS specific variables
party_url <- "https://vasemmisto.fi/wp-json/wp_query/args/"
party_id <- 1292
country_name <- "Finland"

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from API with pagination
extract_dlvas_press_links <- function() {
  all_links <- character(0)
  offset <- 0
  page_size <- 50
  
  repeat {
    tryCatch({
      # Make GET request with query parameters
      response <- GET(
        url = party_url,
        query = list(
          `_embed` = "",
          post_type = "posts",
          posts_per_page = page_size,
          offset = offset,
          lang = "fi"
        )
      )
      
      # Parse JSON response
      json_data <- fromJSON(rawToChar(response$content))
      
      # Break if no more posts
      if(length(json_data) == 0) {
        break
      }
      
      # Extract links from the JSON structure
      links <- json_data$link
      
      # Add new links to collection
      all_links <- c(all_links, links)
      
      message(sprintf("Fetched %d links (offset: %d)", length(links), offset))
      
      # Increment offset
      offset <- offset + page_size
      
      # Rate limiting
      Sys.sleep(1)
      
    }, error = function(e) {
      warning(sprintf("Error extracting links at offset %d: %s", offset, e$message))
      break
    })
  }
  
  # Return unique, non-NA links
  unique(all_links[!is.na(all_links)])
}

# Function to process a single press release
process_dlvas_press_release <- function(url) {
  tryCatch({
    # Read the page
    page <- read_html(url)
    
    # Extract title
    title <- page %>%
      html_node(".mission-title") %>%
      html_text() %>%
      str_replace_all("&shy;", "") %>%  # Remove soft hyphens
      trimws()
    
    # Extract date
    date_text <- page %>%
      html_node(".post-meta strong:first-child") %>%
      html_text() %>%
      str_extract("\\d{2}\\.\\d{2}\\.\\d{4}")
    
    # Convert Finnish date format to Date object
    date <- if(!is.na(date_text)) {
      parts <- str_split(date_text, "\\.")[[1]]
      as.Date(sprintf("%s-%s-%s", parts[3], parts[2], parts[1]))
    } else {
      NA
    }
    
    # Extract text content
    text_parts <- c(
      # Get pretext/summary
      page %>% 
        html_node(".article-pretext") %>%
        html_text() %>%
        trimws(),
      
      # Get main content
      page %>%
        html_nodes(".article-body p") %>%
        html_text() %>%
        trimws() %>%
        .[nchar(.) > 0] %>%  # Remove empty paragraphs
        .[!str_detect(., "^(Jaa artikkeli:|Arvoisa puhemies$)")] # Remove sharing text
    )
    
    # Combine text parts
    text <- paste(text_parts, collapse = "\n\n")
    
    # Return as tibble row
    if(!is.na(title) && !is.na(date) && !is.na(text)) {
      tibble(
        title = title,
        date = date,
        text = text,
        url = url,
        party = "DLVAS",
        country = "Finland",
        year = year(date),
        parlgov_party_id = 1292,
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
extract_dlvas_press_releases <- function() {
  # Set up output file path
  output_file <- file.path("press_release_data", "finland_1292.csv")
  
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
  links <- extract_dlvas_press_links()
  message(sprintf("Found %d total links", length(links)))
  
  # Process new links
  new_links <- setdiff(links, existing_urls)
  if(length(new_links) > 0) {
    message(sprintf("Processing %d new links", length(new_links)))
    for(url in new_links) {
      message(sprintf("Processing press release: %s", url))
      result <- process_dlvas_press_release(url)
      
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
press_releases <- extract_dlvas_press_releases() 

