library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)
library(xml2)
library(jsonlite)
library(here)

# Set SAP specific variables
party_url <- "https://www.socialdemokraterna.se/aktuellt/"
ajax_url <- "https://www.socialdemokraterna.se/4.593ea2c316ae546ed7c9f3d/12.76dd5a4c1811b7aecbd2bb.htm"
party_id <- 904
country_name <- "Sweden"

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract press release links from AJAX response
extract_sap_press_links <- function(content) {
  tryCatch({
    # Get all news items
    articles <- content %>% 
      html_nodes("a.subheading")
    
    # Initialize empty vectors
    links <- character()

    for (article in articles) {
      # Get the link and title
      link_node <- article
      
      if (!is.null(link_node)) {
        link <- link_node %>% html_attr("href")

        
        if (!is.na(link)) {
          links <- c(links, link)
        }
      }
    }
    
    # Create absolute URLs if needed
    urls <- ifelse(
      startsWith(links, "/"),
      paste0("https://www.socialdemokraterna.se", links),
      links
    )
    
    # Create a dataframe with all the information
    results <- tibble(
      url = urls
    )
    
    return(results)
    
  }, error = function(e) {
    warning("Error extracting links: ", e$message)
    return(tibble(
      url = character()
    ))
  })
}

# Function to get pagination content
get_pagination_content <- function(start_hit) {
  tryCatch({
    # Construct query parameters
    params <- list(
      state = "executePaging",
      isRenderingAjaxPagingResult = "true",
      query = "*:*",
      startAtHit = as.character(start_hit)
    )
    
    # Make the request
    response <- GET(
      ajax_url,
      query = params,
      add_headers(
        "Accept" = "text/html",
        "X-Requested-With" = "XMLHttpRequest"
      )
    )
    
    # Check if request was successful
    if (status_code(response) != 200) {
      warning("Failed to get pagination content for offset ", start_hit, 
              ". Status code: ", status_code(response))
      return(NULL)
    }
    
    # Parse the HTML content
    content <- content(response, "text")
    html <- read_html(content)
    
    return(html)
  }, error = function(e) {
    warning("Error getting pagination content for start_hit ", start_hit, ": ", e$message)
    return(NULL)
  })
}

# Function to process a single press release
process_sap_press_release <- function(url) {
  tryCatch({
    Sys.sleep(1)  # Rate limiting
    page <- read_html(url)
    
    # Extract title
    title <- page %>%
      html_node("h1") %>%
      html_text(trim = TRUE)
    
    # Extract date
    date <- page %>%
      html_node("time") %>%
      html_attr("datetime") %>%
      substr(1, 10) %>%  # Extract YYYY-MM-DD part
      as.Date()
    
    # Extract text content
    text <- page %>%
      html_nodes(".sv-text-portlet-content p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    text <- str_remove_all(text, 'Markera en text för att lyssna och översätta vid avsnittet eller klicka direkt på lyssna.\n\nKlicka på \"Lyssna\" för att läsa upp sidans innehåll. Du kan även markera ett avsnitt för att få det uppläst.\n\nÖversätt text och få den uppläst på ett annat språk. Markera texten och välj ”Översättning” vid avsnittet.\n\nGenom att aktivera vårt inbyggda hjälpmedel kan du:\n\n')
    text <- str_remove_all(text, '\n\nKontakta ossSocialdemokraternaPartistyrelsens kansli105 60 Stockholm\n\nVäxel: 08-700 26 00')
    
    
    # Return as tibble row
    tibble(
      title = title,
      date = date,
      text = text,
      url = url
    )
    
  }, error = function(e) {
    warning("Error processing URL ", url, ": ", e$message)
    return(NULL)
  })
}

# Phase 1: Gather all links through pagination
gather_press_release_links <- function() {
  # Define pagination file path
  pagination_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  
  # Try to load existing pagination data
  if (file.exists(pagination_file)) {
    message("Loading existing pagination data...")
    all_links <- readRDS(pagination_file)
    last_hit <- max(all_links$start_hit)
    message(sprintf("Found existing data with %d links, last start_hit: %d", 
                   nrow(all_links), last_hit))
    start_hit <- last_hit + 5  # Increment by 5
  } else {
    message("No existing pagination data found. Starting fresh...")
    all_links <- tibble(
      url = character(),
      title = character(),
      date = as.Date(character()),
      start_hit = integer()
    )
    start_hit <- 0
  }
  
  has_more <- TRUE
  
  while(has_more) {
    message("Processing pagination start_hit: ", start_hit)
    
    # Get pagination content
    content <- get_pagination_content(start_hit)
    
    if (is.null(content)) {
      message("Failed to get content for start_hit ", start_hit)
      break
    }
    
    # Extract press releases from this page
    page_releases <- extract_sap_press_links(content)
    
    if (nrow(page_releases) == 0) {
      message("No more press releases found at start_hit ", start_hit)
      has_more <- FALSE
      break
    }
    
    # Add pagination metadata
    page_releases$start_hit <- start_hit
    page_releases$timestamp <- Sys.time()
    
    # Add to our collection of links
    all_links <- bind_rows(all_links, page_releases)
    message(sprintf("Found %d links on this page. Total links: %d", 
                   nrow(page_releases), nrow(all_links)))
    
    # Save progress after each page
    saveRDS(all_links, pagination_file)
    message(sprintf("Updated pagination file with %d total links", nrow(all_links)))
    
    # Increment start_hit
    start_hit <- start_hit + 5
    
    # Rate limiting between pages
    Sys.sleep(2)
  }
  
  return(all_links)
}

# Phase 2: Process press releases
process_press_releases <- function() {
  # Load pagination data
  pagination_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  if (!file.exists(pagination_file)) {
    stop("No pagination data found. Please run gather_press_release_links first.")
  }
  
  all_links <- readRDS(pagination_file)
  message(sprintf("Loaded %d links from pagination file", nrow(all_links)))
  
  # Set up output file
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  
  # Initialize or load existing results
  if (file.exists(output_file)) {
    message("Loading existing press release data...")
    results <- read_csv(output_file)
    # Filter out already processed URLs
    all_links <- all_links %>%
      filter(!url %in% results$url)
    message(sprintf("Found %d existing press releases", nrow(results)))
    message(sprintf("%d new links to process", nrow(all_links)))
  } else {
    results <- tibble(
      title = character(),
      date = as.Date(character()),
      text = character(),
      url = character(),
      party = character(),
      country = character(),
      year = integer(),
      parlgov_party_id = integer(),
      date_collected = as.Date(character())
    )
    write_csv(results, output_file)  # Create the file
  }
  
  # Process each link
  for (i in 1:nrow(all_links)) {
    url <- all_links$url[i]
    
    # Skip if URL already processed (double-check)
    if (url %in% results$url) {
      message(sprintf("Skipping already processed URL (%d/%d): %s", 
                     i, nrow(all_links), url))
      next
    }
    
    message(sprintf("Processing release (%d/%d): %s", 
                   i, nrow(all_links), url))
    
    release <- process_sap_press_release(url)
    
    if (!is.null(release)) {
      # Add party specific information
      release <- release %>%
        mutate(
          party = "SAP",
          country = country_name,
          year = year(date),
          parlgov_party_id = party_id,
          date_collected = Sys.Date()
        )
      
      # Append to file directly instead of keeping in memory
      write_csv(release, output_file, append = TRUE)
      message("Saved press release to file")
    } else {
      message(sprintf("Failed to process URL (%d/%d): %s", 
                     i, nrow(all_links), url))
    }
    
    # Rate limiting between requests
    Sys.sleep(1)
  }
  
  # Return final results
  message("Reading final results...")
  final_results <- read_csv(output_file)
  return(final_results)
}

# Main execution function
scrape_sap_press_releases <- function(phase = c("both", "links", "process")) {
  phase <- match.arg(phase)
  
  if (phase %in% c("both", "links")) {
    message("Starting Phase 1: Gathering press release links...")
    links <- gather_press_release_links()
    message(sprintf("Completed Phase 1: Gathered %d links", nrow(links)))
  }
  
  if (phase %in% c("both", "process")) {
    message("Starting Phase 2: Processing press releases...")
    results <- process_press_releases()
    message(sprintf("Completed Phase 2: Processed %d press releases", nrow(results)))
    return(results)
  }
  
  if (phase == "links") {
    return(links)
  }
}

# Execute scraping with option to run specific phase
message("Starting SAP press release scraper...")
press_releases <- scrape_sap_press_releases(phase = "process")

# Final save (although individual releases are saved continuously)
if (!is.null(press_releases) && nrow(press_releases) > 0) {
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  press_releases$text <- str_remove_all(press_releases$text, 'Markera en text för att lyssna och översätta vid avsnittet eller klicka direkt på lyssna.\n\nKlicka på \"Lyssna\" för att läsa upp sidans innehåll. Du kan även markera ett avsnitt för att få det uppläst.\n\nÖversätt text och få den uppläst på ett annat språk. Markera texten och välj ”Översättning” vid avsnittet.\n\nGenom att aktivera vårt inbyggda hjälpmedel kan du:\n\n')
  press_releases$text <- str_remove_all(press_releases$text, '\n\nKontakta ossSocialdemokraternaPartistyrelsens kansli105 60 Stockholm\n\nVäxel: 08-700 26 00')
  write_csv(press_releases, output_file)
  message(sprintf("Completed scraping with %d press releases saved to %s", 
                 nrow(press_releases), output_file))
} else {
  message("No press releases were collected")
} 