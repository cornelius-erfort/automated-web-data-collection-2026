library(tidyverse)
library(httr)
library(rvest)
library(lubridate)
library(here)

# Set SP specific variables
party_url <- "https://www.sp.nl/nieuws"
party_id <- 357
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
    if (is.null(date_str) || is.na(date_str)) return(NA)
    
    parts <- str_match(date_str, "([0-9]{1,2})\\s+([a-z]+)\\s+([0-9]{4})")
    if (is.na(parts[1])) return(NA)
    
    day <- sprintf("%02d", as.numeric(parts[2]))
    month <- dutch_months[tolower(parts[3])]
    year <- parts[4]
    
    date <- as.Date(paste(year, month, day, sep = "-"))
    return(date)
  }, error = function(e) {
    warning("Error converting date: ", date_str, " - ", e$message)
    return(NA)
  })
}


# Function to extract press release links from a page
extract_sp_press_links <- function(page_number) {
  tryCatch({
    # Construct URL with page number
    url <- sprintf("%s?page=%d#news-list", party_url, page_number)
    message(sprintf("Fetching URL: %s", url))
    
    # Read the page
    page <- read_html(url)
    
    # Extract links from news items
    links <- page %>%
      html_nodes("a.group.items-stretch") %>%
      html_attr("href")
    
    # Make links absolute and unique
    links <- if(length(links) > 0) {
      unique(paste0("https://www.sp.nl", links))
    } else {
      character(0)
    }
    
    message(sprintf("Found %d unique links on page %d", length(links), page_number))
    return(links)
    
  }, error = function(e) {
    warning(sprintf("Error extracting links from page %d: %s", page_number, e$message))
    return(character(0))
  })
}

# Function to process a single press release
process_sp_press_release <- function(url) {
  tryCatch({
    # Read the page
    page <- read_html(url)
    
    # Extract title
    title <- page %>%
      html_node("h2") %>%
      html_text() %>%
      trimws()
    
    # Extract date
    date_text <- page %>%
      html_node(".text-subtitle.text-primary") %>%
      html_text() %>%
      trimws()
    
    # Convert date (format: "13 februari '25")
    date <- date_text %>%
      gsub("'", "20", .) %>%  # Replace ' with 20 for year
      convert_dutch_date()
    
    # Extract text content
    text <- page %>%
      html_node(".prose") %>%
      html_text() %>%
      trimws()
    
    # Return as tibble row
    if(!is.na(title) | !is.na(date) | !is.na(text)) {
      tibble(
        title = title,
        date = date,
        text = text,
        url = url,
        party = "SP",
        country = "Netherlands",
        year = year(date),
        parlgov_party_id = 357,
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
extract_sp_press_releases <- function() {
  # Set up output file paths
  output_file <- file.path("press_release_data", "netherlands_357.csv")
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
      links <- extract_sp_press_links(page_number)
      
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
      
      # Check for duplicate links before adding
      new_links <- setdiff(links, all_links)
      if(length(new_links) == 0) {
        message("No new links found. Assuming end of pagination.")
        pagination_data$completed <- TRUE
        break
      }
      
      # Process new links from this page immediately
      message(sprintf("Processing %d new links from page %d", length(new_links), page_number))
      for(url in new_links) {
        message(sprintf("Processing press release: %s", url))
        result <- process_sp_press_release(url)
        
        if(!is.null(result)) {
          # Append to CSV
          write_csv(result, output_file, append = TRUE)
          message(sprintf("Saved press release: %s", result$title))
        }
        
        # Sys.sleep(1)  # Rate limiting between press releases
      }
      
      # Update all_links after processing
      all_links <- c(all_links, new_links)
      message(sprintf("Added %d new links from page %d to pagination data", length(new_links), page_number))
      
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
  } else {
    message("Using previously completed pagination data")
    all_links <- pagination_data$all_links
    
    # Process any remaining unprocessed links
    new_urls <- setdiff(all_links, existing_urls)
    if(length(new_urls) > 0) {
      message(sprintf("Processing %d remaining unprocessed links", length(new_urls)))
      for(url in new_urls) {
        message(sprintf("Processing press release: %s", url))
        result <- process_sp_press_release(url)
        
        if(!is.null(result)) {
          write_csv(result, output_file, append = TRUE)
          message(sprintf("Saved press release: %s", result$title))
        }
        
        # Sys.sleep(1)
      }
    }
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
press_releases <- extract_sp_press_releases()

# Read pagination data from rds
