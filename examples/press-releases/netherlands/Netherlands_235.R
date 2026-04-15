library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)
library(xml2)
library(jsonlite)
library(here)

# Set CDA specific variables
party_url <- "https://www.cda.nl/actueel/nieuws/"
party_id <- 235
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
    # Extract components using regex
    if (is.null(date_str) || is.na(date_str)) return(NA)
    
    parts <- str_match(date_str, "([0-9]{1,2})\\s+([a-z]+)\\s+([0-9]{4})")
    if (is.na(parts[1])) return(NA)
    
    day <- sprintf("%02d", as.numeric(parts[2]))
    month <- dutch_months[tolower(parts[3])]
    year <- parts[4]
    
    # Combine into ISO format
    date <- as.Date(paste(year, month, day, sep = "-"))
    return(date)
  }, error = function(e) {
    warning("Error converting date: ", date_str, " - ", e$message)
    return(NA)
  })
}

# Function to extract press release links from a page
extract_cda_press_links <- function(content) {
  tryCatch({

    # Get all article links
    articles <- content %>% html_nodes("article")
    
    # Initialize empty vector for collecting data
    links <- character()

    for (article in articles) {
    
      
      # Get the link
      link <- article %>% 
        html_node("a") %>% 
        html_attr("href")
      
      if (!is.na(link)) {
        links <- c(links, link)
      }
    }
    
    # Create absolute URLs if needed
    urls <- ifelse(
      startsWith(links, "/"),
      paste0("https://www.cda.nl", links),
      links
    )
    
    # Create a dataframe with all the information
    results <- tibble(
      url = urls,
    )
    
    return(results)
    
  }, error = function(e) {
    warning("Error extracting links from ", page_url, ": ", e$message)
    return(tibble(url = character()))
  })
}

# Function to process a single press release
process_cda_press_release <- function(url) {
  tryCatch({
    Sys.sleep(1)  # Rate limiting
    page <- read_html(url)
    
    # Extract title
    title <- page %>%
      html_node("h1") %>%
      html_text(trim = TRUE)
    
    # Extract and convert date
    date_str <- page %>%
      html_node(".text-16.flex.items-center.gap-8.mb-16") %>%
      html_text(trim = TRUE) %>%
      str_extract("[0-9]{1,2}\\s+[a-z]{3,}\\s+[0-9]{4}")
    
    date <- convert_dutch_date(date_str)
    
    # Extract text content - combine intro paragraph and main content
    intro_text <- page %>%
      html_node(".font-sans.font-normal.text-white.mb-40") %>%
      html_text(trim = TRUE)
    
    main_text <- page %>%
      html_nodes(".chunks p.font-sans.font-normal.text-blue") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    # Get any blockquotes
    quotes <- page %>%
      html_nodes("blockquote p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    # Combine all text parts
    text <- paste(
      intro_text,
      main_text,
      if (nchar(quotes) > 0) paste("\nCitaten:\n", quotes) else "",
      sep = "\n\n"
    ) %>%
      str_trim()
    
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

# Function to get the sprig:config from the base page
get_sprig_config <- function(base_url) {
  tryCatch({
    page <- read_html(base_url)
    
    # Extract the sprig:config from the data-hx-vals attribute
    sprig_component <- page %>%
      html_node(".sprig-component") %>%
      html_attr("data-hx-vals")
    
    if (is.null(sprig_component) || sprig_component == "") {
      stop("Could not find sprig:config in the page")
    }
    
    # Extract the sprig:config value using regex
    config_match <- str_match(sprig_component, '"sprig:config":"([^"]+)"')
    if (is.na(config_match[1])) {
      stop("Could not extract sprig:config value")
    }
    
    return(config_match[2])
  }, error = function(e) {
    warning("Error getting sprig:config: ", e$message)
    return(NULL)
  })
}

# Function to get pagination content
get_pagination_content <- function(offset, sprig_config) {
  tryCatch({
    # Base URL for pagination
    base_url <- "https://www.cda.nl/index.php/actions/sprig-core/components/render/"
    
    # Construct the JSON part (without escaping)
    json_part <- '{"id":"component-hclgsy","siteId":1,"template":"_components\\/sprig\\/newsIndex"}'
    
    # Construct the full URL with proper parameters
    url <- paste0(
      base_url, 
      "?offset=", offset, 
      "&filteroptags=&filteropthema=",
      "&sprig:config=", sprig_config %>% str_extract(".*(?=\\{)"), json_part
    )
    
    # Make the request
    response <- GET(url)
    
    # Check if request was successful
    if (status_code(response) != 200) {
      warning("Failed to get pagination content for offset ", offset, 
              ". Status code: ", status_code(response))
      return(NULL)
    }
    
    # Parse the HTML content
    content <- content(response, "text")
    html <- read_html(content)
    
    return(html)
  }, error = function(e) {
    warning("Error getting pagination content for offset ", offset, ": ", e$message)
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
    last_offset <- max(all_links$offset)
    message(sprintf("Found existing data with %d links, last offset: %d", 
                   nrow(all_links), last_offset))
    offset <- last_offset + 16  # Start from next offset
  } else {
    message("No existing pagination data found. Starting fresh...")
    all_links <- tibble(url = character())
    offset <- 0
  }
  
  # Get sprig config from base page
  message("Getting sprig config from base page...")
  sprig_config <- get_sprig_config("https://www.cda.nl/nieuws/")
  message("Found sprig config: ", substr(sprig_config, 1, 50), "...")
  
  has_more <- TRUE
  
  while(has_more) {
    message("Processing pagination offset: ", offset)
    
    # Get pagination content with config
    content <- get_pagination_content(offset, sprig_config)
    
    if (is.null(content)) {
      message("Failed to get content for offset ", offset)
      break
    }
    
    # Extract press releases from this page
    page_releases <- extract_cda_press_links(content)
    page_releases$offset <- offset
    page_releases$timestamp <- Sys.time()
    
    # Check if we got any results
    if (nrow(page_releases) == 0) {
      message("No more press releases found at offset ", offset)
      has_more <- FALSE
      break
    }
    
    # Add to our collection of links
    all_links <- bind_rows(all_links, page_releases)
    message(sprintf("Found %d links on this page. Total links: %d", 
                   nrow(page_releases), nrow(all_links)))
    
    # Save progress after each page
    saveRDS(all_links, pagination_file)
    message(sprintf("Updated pagination file with %d total links", nrow(all_links)))
    
    # Increment offset
    offset <- offset + 16
    
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
    write_csv(results, output_file)  # Create the file
  }
  
  # Process each link
  for (i in 1:nrow(all_links)) {
    url <- all_links$url[i]
    
    # Check if URL already processed
    if (url %in% results$url) {
      message(sprintf("Skipping already processed URL (%d/%d): %s", 
                     i, nrow(all_links), url))
      next
    }
    
    message(sprintf("Processing release (%d/%d): %s", 
                   i, nrow(all_links), url))
    
    release <- process_cda_press_release(url)
    
    if (!is.null(release)) {
      # Add party specific information
      release <- release %>%
        mutate(
          party = "CDA",
          country = country_name,
          year = year(date),
          parlgov_party_id = party_id,
          date_collected = Sys.Date()
        )
      
      # Append to file directly instead of keeping in memory
      write_csv(release, output_file, append = TRUE)
      message("Saved press release to file")
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
scrape_cda_press_releases <- function(phase = c("both", "links", "process")) {
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
message("Starting CDA press release scraper...")
press_releases <- scrape_cda_press_releases(phase = "both")

# Final save
if (nrow(press_releases) > 0) {
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  write_csv(press_releases, output_file)
  message(sprintf("Saved %d press releases to %s", 
                 nrow(press_releases), output_file))
} else {
  message("No press releases found")
} 