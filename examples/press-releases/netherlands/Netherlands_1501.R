library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)
library(xml2)
library(jsonlite)
library(here)

# Set PVV specific variables
party_url <- "https://www.pvv.nl/in-de-media/persberichten.html"
party_id <- 1501
country_name <- "Netherlands"

# Add new URL for web archive
archive_url <- "https://www.pvv.nl/webarchief.html"

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
extract_pvv_press_links <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Get all articles
    articles <- page %>% 
      html_nodes("article.uk-article")
    
    # Initialize empty vectors
    links <- character()
    titles <- character()
    dates <- character()
    
    for (article in articles) {
      # Get the permalink from data-permalink attribute
      link <- article %>% 
        html_attr("data-permalink")
      
      # Get the title
      title <- article %>%
        html_node("meta[property='name']") %>%
        html_attr("content")
      
      # Get the date
      date <- article %>%
        html_node("meta[property='datePublished']") %>%
        html_attr("content")
      
      if (!is.na(link)) {
        links <- c(links, link)
        titles <- c(titles, title)
        dates <- c(dates, date)
      }
    }
    
    # Create a dataframe
    results <- tibble(
      url = links,
      title = titles,
      date_str = dates
    )
    
    return(results)
    
  }, error = function(e) {
    warning("Error extracting links from ", url, ": ", e$message)
    return(tibble(
      url = character(),
      title = character(),
      date_str = character()
    ))
  })
}

# Function to extract press release links from archive page
extract_pvv_archive_links <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Get all article cards
    articles <- page %>% 
      html_nodes("a.uk-card")
    
    # <a class="uk-card uk-card-default uk-card-small uk-card-hover uk-link-toggle" href="/nieuws/geert-wilders/11372-extra-geld-voor-defensie-bespreekbaar-maar-ook-lagere-lasten-voor-de-burger.html">                                                                                                      <div class="uk-card-media-top">                       <picture> <source type="image/webp" srcset="/templates/yootheme/cache/a7/Logo_website-a7abbffd.webp 768w, /templates/yootheme/cache/7d/Logo_website-7d0500b0.webp 960w" sizes="(min-width: 960px) 960px"> <img src="/templates/yootheme/cache/80/Logo_website-80d6737c.jpeg" width="960" height="540" alt="" loading="lazy" class="el-image"> </picture>                       </div>                                                 <div class="uk-card-body uk-margin-remove-first-child">                                                                <h3 class="el-title uk-card-title uk-margin-top uk-margin-remove-bottom">                        Extra geld voor Defensie bespreekbaar, maar ook lagere lasten voor de burger                    </h3>                                                         <div class="el-content uk-panel uk-margin-top">Geert Wilders: ‘De PVV wil miljarden extra voor lagere huren en energierekening en goedkopere boo...</div>                                                            </div>                                                                </a>
    
    # Initialize empty vectors
    links <- character()
    titles <- character()
    # dates <- character()
    
    for (article in articles) {
      # Get the link
      link <- article %>% 
        # html_node("a") %>%
        html_attr("href")
      
      # Make link absolute if it's relative
      if (!is.na(link) && startsWith(link, "/")) {
        link <- paste0("https://www.pvv.nl", link)
      }
      
      # Get the title
      title <- article %>%
        html_node("h3.el-title") %>%
        html_text(trim = TRUE)
      
      # Get the date
      # date <- article %>%
      #   html_node(".el-meta") %>%
      #   html_text(trim = TRUE)
      
      if (!is.na(link)) {
        links <- c(links, link)
        titles <- c(titles, title)
        # dates <- c(dates, date)
      }
    }
    
    # Create a dataframe
    results <- tibble(
      url = links,
      title = titles # ,
      # date_str = dates
    )
    
    return(results)
    
  }, error = function(e) {
    warning("Error extracting links from ", url, ": ", e$message)
    return(tibble(
      url = character(),
      title = character(),
      date_str = character()
    ))
  })
}

# Function to process a single press release
process_pvv_press_release <- function(url) {
  tryCatch({
    Sys.sleep(1)  # Rate limiting
    page <- read_html(url)
    
    # Extract title
    title <- page %>%
      html_node("h1.uk-article-title") %>%
      html_text(trim = TRUE)
    
    # Extract date from meta tag
    date_str <- page %>%
      html_node("meta[property='datePublished']") %>%
      html_attr("content")
    
    date <- as.Date(substr(date_str, 1, 10))
    
    # Extract text content
    text <- page %>%
      html_node("div[property='text']") %>%
      html_text(trim = TRUE)
    
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

# Modify gather_press_release_links to handle both sources
gather_press_release_links <- function() {
  # Define pagination file path
  pagination_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
  
  # Try to load existing pagination data
  if (file.exists(pagination_file)) {
    message("Loading existing pagination data...")
    all_links <- readRDS(pagination_file)
    last_start <- max(all_links$start_param)
    message(sprintf("Found existing data with %d links, last start: %d", 
                   nrow(all_links), last_start))
    next_start <- last_start + 5
  } else {
    message("No existing pagination data found. Starting fresh...")
    all_links <- tibble(
      url = character(),
      title = character(),
      date_str = character(),
      start_param = integer(),
      source = character()  # Add source column to track origin
    )
    next_start <- 0
  }
  
  # Function to process pagination for either source
  process_pagination <- function(base_url, extractor_fn, source_name) {
    consecutive_empty_pages <- 0
    max_empty_pages <- 3
    next_start <- 0
    
    while (consecutive_empty_pages < max_empty_pages) {
      message(sprintf("Processing %s page with start=%d", source_name, next_start))
      
      # Construct page URL
      page_url <- sprintf("%s?start=%d", base_url, next_start)
      
      # Extract releases from this page
      page_releases <- extractor_fn(page_url)
      
      if (nrow(page_releases) > 0) {
        consecutive_empty_pages <- 0
        
        # Add metadata
        page_releases <- page_releases %>%
          mutate(
            start_param = next_start,
            timestamp = Sys.time(),
            source = source_name
          )
        
        # Do not add duplicates
        page_releases <- page_releases[!(page_releases$url %in% all_links$url), ]
        
        # Add to collection
        all_links <<- bind_rows(all_links, page_releases)
        message(sprintf("Found %d new links. Total links: %d", 
                       nrow(page_releases), nrow(all_links)))
        
        # Save progress
        saveRDS(all_links, pagination_file)
      } else {
        message(sprintf("No links found at start=%d", next_start))
        consecutive_empty_pages <- consecutive_empty_pages + 1
      }
      
      next_start <- next_start + 5
      Sys.sleep(2)
    }
  }
  
  # Process both sources
  message("Processing press releases...")
  process_pagination(party_url, extract_pvv_press_links, "press_releases")
  
  message("Processing web archive...")
  process_pagination(archive_url, extract_pvv_archive_links, "web_archive")
  
  message("Finished gathering links from both sources")
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
    message(sprintf("Loaded %d existing press releases", nrow(results)))
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
    url <- all_links$url[i]
    
    # Skip if already processed
    if (url %in% results$url) {
      message(sprintf("Skipping already processed URL (%d/%d): %s", 
                     i, nrow(all_links), url))
      next
    }
    
    message(sprintf("Processing release (%d/%d): %s", 
                   i, nrow(all_links), url))
    
    release <- process_pvv_press_release(url)
    
    if (!is.null(release)) {
      # Add party specific information
      release <- release %>%
        mutate(
          party = "PVV",
          country = country_name,
          year = year(date),
          parlgov_party_id = party_id,
          date_collected = Sys.Date()
        )
      
      # Append to file
      write_csv(release, output_file, append = TRUE)
      message("Saved press release to file")
    }
    
    Sys.sleep(1)  # Rate limiting
  }
  
  # Return final results
  message("Reading final results...")
  final_results <- read_csv(output_file)
  return(final_results)
}

# Main execution function
scrape_pvv_press_releases <- function(phase = c("both", "links", "process")) {
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

# Execute scraping
message("Starting PVV press release scraper...")
press_releases <- scrape_pvv_press_releases(phase = "process") 
