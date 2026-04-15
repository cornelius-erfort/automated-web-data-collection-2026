library(tidyverse)
library(httr)
library(rvest)
library(lubridate)
library(here)

# Set SDP specific variables
party_url <- "https://www.sdp.fi/ajankohtaista/page/"
party_id <- 395
country_name <- "Finland"

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from a page
extract_sdp_press_links <- function(page_number) {
  tryCatch({
    # Construct URL with page number
    url <- paste0(party_url, page_number, "/")
    message(sprintf("Fetching URL: %s", url))
    
    # Read the page
    page <- read_html(url)
    
    # Extract links from news items
    links <- page %>%
      html_nodes("a.home-item__title") %>%
      html_attr("href")
    
    # Return unique links
    unique(links)
    
  }, error = function(e) {
    warning(sprintf("Error extracting links from page %d: %s", page_number, e$message))
    return(character(0))
  })
}

# Function to process a single press release
process_sdp_press_release <- function(url) {
  tryCatch({
    # Read the page
    page <- read_html(url)
    
    # Extract title
    title <- page %>%
      html_node(".hero-single__title") %>%
      html_text() %>%
      trimws()
    
    # Extract date
    date_text <- page %>%
      html_node(".hero-single__date") %>%
      html_text() %>%
      trimws() %>%
      str_extract("\\d{1,2}\\.\\d{1,2}\\.\\d{4}") # Extract date in format dd.mm.yyyy
    
    # Convert Finnish date format to Date object
    date <- if(!is.na(date_text)) {
      parts <- str_split(date_text, "\\.")[[1]]
      as.Date(sprintf("%s-%s-%s", parts[3], parts[2], parts[1]))
    } else {
      NA
    }
    
    # Extract text content
    text_parts <- c(
      page %>% html_node(".hero-single__lead") %>% html_text() %>% trimws(),
      page %>% 
        html_nodes(".layout-texteditor p") %>%
        html_text() %>%
        trimws() %>%
        .[nchar(.) > 0]  # Remove empty paragraphs
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
        party = "SDP",
        country = "Finland",
        year = year(date),
        parlgov_party_id = 395,
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
extract_sdp_press_releases <- function() {
  # Set up output file paths
  output_file <- file.path("press_release_data", "finland_395.csv")
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
  
  # Initialize or load pagination data
  pagination_data <- if(file.exists(pagination_file)) {
    message("Loading existing pagination data...")
    readRDS(pagination_file)
  } else {
    list(
      last_page = 1,
      all_links = character(0),
      completed = FALSE
    )
  }
  
  # Continue from last page if not completed
  if(!pagination_data$completed) {
    page_number <- pagination_data$last_page
    all_links <- pagination_data$all_links
    same_count <- 0
    max_attempts <- 3
    previous_links <- NULL
    
    # Get all press release links with pagination
    repeat {
      links <- extract_sdp_press_links(page_number)
      
      # Check if we're getting the same links repeatedly
      if(identical(links, previous_links)) {
        same_count <- same_count + 1
        if(same_count >= max_attempts) {
          message("Got same links multiple times. Assuming end of pagination.")
          pagination_data$completed <- TRUE
          break
        }
      } else {
        same_count <- 0
      }
      previous_links <- links
      
      if(length(links) == 0) {
        message(sprintf("No more links found on page %d. Stopping.", page_number))
        pagination_data$completed <- TRUE
        break
      }
      
      # Process new links from this page immediately
      message(sprintf("Processing %d new links from page %d", length(links), page_number))
      for(url in links) {
        if(!url %in% existing_urls) {
          message(sprintf("Processing press release: %s", url))
          result <- process_sdp_press_release(url)
          
          if(!is.null(result)) {
            # Append to CSV
            write_csv(result, output_file, append = TRUE)
            message(sprintf("Saved press release: %s", result$title))
          }
          
          # Sys.sleep(1)  # Rate limiting between press releases
        }
      }
      
      # Update all_links after processing
      all_links <- c(all_links, links)
      message(sprintf("Added %d links from page %d to pagination data", length(links), page_number))
      
      # Update pagination data and save
      pagination_data$last_page <- page_number + 1
      pagination_data$all_links <- all_links
      saveRDS(pagination_data, pagination_file)
      
      page_number <- page_number + 1
      # Sys.sleep(2)  # Rate limiting between pages
    }
    
    # Save final pagination state
    pagination_data$all_links <- all_links
    saveRDS(pagination_data, pagination_file)
  }
  
  # Read and return final results
  final_results <- read_csv(output_file, show_col_types = FALSE)
  message(sprintf("Total press releases in file: %d", nrow(final_results)))
  
  return(final_results)
}

# Function to reset pagination data (useful for fresh start)
reset_pagination_data <- function() {
  pagination_file <- file.path("meta-data", sprintf("pagination_%d.rds", party_id))
  if(file.exists(pagination_file)) {
    file.remove(pagination_file)
    message(sprintf("Pagination data reset successfully for party %d", party_id))
  }
}

# Run the scraper
press_releases <- extract_sdp_press_releases() 
