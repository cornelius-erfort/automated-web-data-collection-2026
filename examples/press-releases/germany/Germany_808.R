library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(lubridate)
library(xml2)
library(jsonlite)
library(here)

# Set CDU specific variables
force_rescrape <- FALSE
base_url <- "https://www.cducsu.de/presse"  # Initial page URL
api_url <- "https://www.cducsu.de/api/output/views?_wrapper_format=drupal_ajax"
party_id <- 808
country_name <- "Germany"
number_of_items <- 5000  # Set number of items to fetch consistently

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to get CDU press releases via AJAX (single request)
get_cdu_press_releases <- function(number = number_of_items) {
  message(sprintf("Fetching up to %d press releases...", number))
  
  body <- list(
    type = "views",
    views = "press",
    display = "press",
    view_mode = "teaser_game",
    number = number,
    offset = 0,
    "options[filters][type][]" = "press_release",
    "options[append][object]" = "cducsu",
    "options[append][func]" = "masonryAppend",
    "options[append][params][]" = ".mark--press-press-release",
    "options[wrapper]" = "cducsu_masonry_item",
    "_drupal_ajax" = "1"
  )
  
  response <- POST(
    url = api_url,
    body = body,
    encode = "form",
    add_headers(
      "Content-Type" = "application/x-www-form-urlencoded",
      "Accept" = "application/json",
      "X-Requested-With" = "XMLHttpRequest"
    )
  )
  
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content))
    api_response <- content[content$command == "apiResponse", ]
    
    if (!is.null(api_response$data)) {
      html_content <- api_response$data[[1]]$view
      
      urls <- lapply(html_content, function(html_str) {
        doc <- read_html(html_str)
        href <- html_node(doc, "a.game") %>% 
          html_attr("href")
        if (!is.na(href)) paste0("https://www.cducsu.de", href)
      })
      
      valid_urls <- unlist(urls[!sapply(urls, is.null)])
      
      saveRDS(valid_urls, file.path("meta-data", paste0("pagination_", party_id, ".rds")))
      
      message(sprintf("Retrieved %d URLs", length(valid_urls)))
      return(valid_urls)
    }
  }
  
  warning("Failed to get press releases. Status code: ", status_code(response))
  return(NULL)
}

# Function to process CDU press releases
process_press_releases_cdu <- function(urls) {
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  
  if (file.exists(output_file)) {
    results <- read_csv(output_file)
    message("Loaded ", nrow(results), " existing press releases from CSV")
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
  }
  
  remaining_urls <- setdiff(urls, results$url)
  total_remaining <- length(remaining_urls)
  message("Processing ", total_remaining, " remaining press releases...")
  
  for (url in remaining_urls) {
    message("Processing URL: ", url)
    
    tryCatch({
      Sys.sleep(2)
      page <- read_html(url)
      
      title <- page %>%
        html_node("h1.head__headline") %>%
        html_text() %>%
        str_trim()
      
      date <- page %>%
        html_node(".meta__date") %>%
        html_text() %>%
        str_trim() %>%
        dmy()
      
      head_content <- page %>%
        html_nodes(".head__content p") %>%
        html_text() %>%
        str_trim()
      
      main_content <- page %>%
        html_nodes(".wysiwyg p") %>%
        html_text() %>%
        str_trim()
      
      all_content <- c(head_content, main_content)
      text <- all_content[all_content != ""] %>%
        paste(collapse = "\n\n")
      
      if (!is.null(title) && !is.na(date)) {
        new_row <- tibble(
          title = title,
          date = date,
          text = text,
          url = url,
          party = "CDU",
          country = country_name,
          year = year(date),
          parlgov_party_id = party_id,
          date_collected = Sys.Date()
        )
        
        results <- bind_rows(results, new_row)
        write_csv(results, output_file)
        message("Saved progress, processed ", nrow(results), " press releases so far")
      }
    }, error = function(e) {
      message("Error processing URL ", url, ": ", e$message)
    })
  }
  
  return(results)
}

# Main execution
message("Starting CDU press release scraper...")

# Check if we should force rescrape or if pagination file doesn't exist
pagination_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
if (force_rescrape || !file.exists(pagination_file)) {
  urls <- get_cdu_press_releases(number = number_of_items)
} else {
  urls <- readRDS(pagination_file)
  message(sprintf("Loaded %d existing URLs from pagination file", length(urls)))
}

if (!is.null(urls)) {
  message("Processing CDU press releases...")
  press_releases <- process_press_releases_cdu(urls)
  
  # Final deduplication
  message("Performing final deduplication...")
  initial_rows <- nrow(press_releases)
  
  press_releases <- press_releases %>%
    arrange(url, desc(date_collected)) %>%
    distinct(url, .keep_all = TRUE)
  
  final_rows <- nrow(press_releases)
  
  if (initial_rows != final_rows) {
    message(sprintf("Removed %d duplicate entries", initial_rows - final_rows))
  } else {
    message("No duplicates found")
  }
  
  # Save final results
  output_file <- file.path("press_release_data", 
                          paste0(tolower(country_name), "_", party_id, ".csv"))
  write_csv(press_releases, output_file)
  message(sprintf("Saved %d press releases to %s", nrow(press_releases), output_file))
} else {
  message("No URLs available, cannot process press releases")
} 