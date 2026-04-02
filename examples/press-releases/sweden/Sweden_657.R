library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(rvest)
library(xml2)
library(stringr)

# Set constants
party_id <- 657
country_name <- "Sweden"
base_url <- "https://moderaterna.se"
ajax_url <- "https://moderaterna.se/wp-json/v1/articles"
news_url <- "https://moderaterna.se/nyheter/"

# Create directories if they don't exist
dir.create("meta-data", showWarnings = FALSE)
dir.create("press_release_data", showWarnings = FALSE)

# Function to extract X-WP-Nonce from the news page
get_wp_nonce <- function() {
  page <- read_html(news_url)
  script_content <- page %>%
    html_node("script#hiq-scripts-js-extra") %>%
    html_text()
  
  # Extract the nonce using regex
  nonce <- str_match(script_content, '"restNonce":"(.*?)"')[,2]
  return(nonce)
}

# Function to extract press release links from JSON response
extract_m_press_links <- function(json_data) {
  if (length(json_data$posts) == 0) {
    return(NULL)
  }
  
  results <- tibble(
    url = paste0(base_url, json_data$posts$post_permalink),
    title = json_data$posts$post_title,
    date = ymd_hms(json_data$posts$post_date_gmt) %>% as.Date(),
    content = json_data$posts$post_content
  )
  
  return(results)
}

# Function to process individual press release from JSON data
process_m_press_release <- function(post_data) {
  tryCatch({
    # Determine if content is HTML
    if (is.character(post_data$content) && grepl("<.*?>", post_data$content)) {
      # Parse the HTML content
      text <- post_data$content %>%
        read_html() %>%
        html_text(trim = TRUE) %>%
        str_replace_all("<!--.*?-->", "") %>%
        str_trim()
    } else {
      # Use the raw text content
      text <- post_data$content %>%
        str_replace_all("<!--.*?-->", "") %>%
        str_trim()
    }
    
    if (nchar(text) < 50) {
      return(NULL)
    }
    
    result <- tibble(
      title = post_data$title,
      date = post_data$date,
      text = text,
      url = post_data$url
    )
    
    return(result)
    
  }, error = function(e) {
    message(sprintf("Error processing post: %s - %s", post_data$url, e$message))
    return(NULL)
  })
}

# Function to gather press release links
gather_press_release_links <- function() {
  pagination_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  
  # Load existing pagination data if available
  if (file.exists(pagination_file)) {
    all_links <- readRDS(pagination_file)
    loaded_posts <- nrow(all_links)
    message(sprintf("Loaded %d existing links", loaded_posts))
  } else {
    all_links <- tibble()
    loaded_posts <- 0
  }
  
  has_more <- TRUE
  
  # Get the X-WP-Nonce
  wp_nonce <- get_wp_nonce()
  
  while (has_more) {
    message(sprintf("Fetching posts starting from offset %d", loaded_posts))
    
    # Define headers for requests
    request_headers <- c(
      "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:136.0) Gecko/20100101 Firefox/136.0",
      "Accept" = "application/json, text/plain, */*",
      "Accept-Language" = "en-US,en;q=0.5",
      "Accept-Encoding" = "gzip, deflate, br, zstd",
      "Referer" = news_url,
      "Connection" = "keep-alive",
      "Sec-Fetch-Dest" = "empty",
      "Sec-Fetch-Mode" = "no-cors",
      "Sec-Fetch-Site" = "same-origin",
      "TE" = "trailers",
      "X-WP-Nonce" = wp_nonce,
      "Alt-Used" = "moderaterna.se",
      "Priority" = "u=0",
      "Pragma" = "no-cache",
      "Cache-Control" = "no-cache"
    )
    
    # Make request to AJAX endpoint with full headers
    response <- GET(
      ajax_url,
      query = list(
        loadedPosts = loaded_posts,
        firstTime = "false",
        subject = ""
      ),
      add_headers(.headers = request_headers)
    )
    
    if (status_code(response) != 200) {
      message(sprintf("Error fetching data from API: %s", status_code(response)))
      break
    }
    
    json_data <- fromJSON(rawToChar(response$content))
    new_links <- extract_m_press_links(json_data)
    
    if (is.null(new_links) || nrow(new_links) == 0) {
      has_more <- FALSE
      break
    }
    
    all_links <- bind_rows(all_links, new_links)
    loaded_posts <- nrow(all_links)
    
    # Save progress
    saveRDS(all_links, pagination_file)
    message(sprintf("Saved %d links to file", nrow(all_links)))
    
    # Rate limiting
    Sys.sleep(1)
  }
  
  return(all_links)
}

# Function to process press releases
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
    write_csv(results, output_file)
  }
  
  # Process each link
  for (i in 1:nrow(all_links)) {
    post_data <- all_links[i, ]
    
    message(sprintf("Processing release (%d/%d): %s", 
                   i, nrow(all_links), post_data$url))
    
    release <- process_m_press_release(post_data)
    
    if (!is.null(release)) {
      # Add party specific information
      release <- release %>%
        mutate(
          party = "M",
          country = country_name,
          year = year(date),
          parlgov_party_id = party_id,
          date_collected = Sys.Date()
        )
      
      # Append to file
      write_csv(release, output_file, append = TRUE)
      message("Saved press release to file")
    }

  }
}

# Main execution function
scrape_m_press_releases <- function(phase = c("both", "links", "process")) {
  phase <- match.arg(phase)
  
  if (phase %in% c("both", "links")) {
    message("Starting Phase 1: Gathering press release links...")
    links <- gather_press_release_links()
    message(sprintf("Completed Phase 1: Gathered %d links", nrow(links)))
  }
  
  if (phase %in% c("both", "process")) {
    message("Starting Phase 2: Processing press releases...")
    process_press_releases()
    message("Completed Phase 2")
  }
}

# Execute scraping
scrape_m_press_releases(phase = "both") 
