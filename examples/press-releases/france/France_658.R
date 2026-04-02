library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

# Set Les Républicains specific variables
ajax_url <- "https://republicains.fr/wp-admin/admin-ajax.php"
party_id <- 658
country_name <- "France"
party_name <- "Les Républicains"
parlgov_party_id <- 658

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to extract nonce from the main page
get_nonce <- function(url) {
  tryCatch({
    page <- read_html(url)
    nonce <- page %>%
      html_node("div.bt_bb_masonry_post_grid_content") %>%
      html_attr("data-bt-bb-masonry-post-grid-nonce")
    return(nonce)
  }, error = function(e) {
    stop("Error extracting nonce: ", e$message)
  })
}

# Function to send POST request to AJAX endpoint
fetch_pagination_data <- function(ajax_url, offset, nonce) {
  tryCatch({
    response <- POST(
      ajax_url,
      body = list(
        action = "bt_bb_get_grid",
        number = 1000,
        category = "",  # Assuming no specific category filter
        show = "a%3A6%3A%7Bs%3A8%3A%22category%22%3Bb%3A1%3Bs%3A4%3A%22date%22%3Bb%3A1%3Bs%3A6%3A%22author%22%3Bb%3A0%3Bs%3A8%3A%22comments%22%3Bb%3A0%3Bs%3A7%3A%22excerpt%22%3Bb%3A1%3Bs%3A5%3A%22share%22%3Bb%3A1%3B%7D",
        `bt-bb-masonry-post-grid-nonce` = nonce,
        `post-type` = "post",
        offset = offset
      ),
      encode = "form"
    )
    
    if (status_code(response) != 200) {
      stop("Failed to fetch data, status code: ", status_code(response))
    }
    
    content <- content(response, "text")
    # Parse the content to extract links
    links <- read_html(content) %>%
      html_nodes("h5.bt_bb_grid_item_post_title a") %>%
      html_attr("href") %>%
      unique()
    
    return(links)
  }, error = function(e) {
    warning("Error fetching pagination data: ", e$message)
    return(character())
  })
}

# Function to translate French dates to English format
translate_french_date <- function(french_date) {
  month_dict <- c(
    "janvier" = "January", "février" = "February", "mars" = "March", 
    "avril" = "April", "mai" = "May", "juin" = "June", 
    "juillet" = "July", "août" = "August", "septembre" = "September", 
    "octobre" = "October", "novembre" = "November", "décembre" = "December"
  )
  
  # Replace French month with English month
  for (french_month in names(month_dict)) {
    english_month <- month_dict[[french_month]]
    french_date <- str_replace(french_date, french_month, english_month)
  }
  
  return(french_date)
}

# Function to scrape detailed content from individual pages
scrape_detailed_content <- function(url) {
  tryCatch({
    page <- read_html(url)
    title <- page %>% html_node("h1.bt_bb_headline_tag span.bt_bb_headline_content span") %>% html_text(trim = TRUE)
    date_french <- page %>% html_node("span.btArticleDate") %>% html_text(trim = TRUE)
    date_english <- translate_french_date(date_french)
    date <- dmy(date_english)
    content <- page %>% html_node("div.btArticleContent") %>% html_text(trim = TRUE)
    list(title = title, date = date, content = content)
  }, error = function(e) {
    message("Error scraping detailed content from ", url, ": ", e$message)
    return(list(title = NA, date = NA, content = NA))
  })
}

# Main execution
message("Starting Les Républicains press release scraper...")

# Extract nonce from the main page
nonce <- get_nonce("https://republicains.fr/actualites/")
message("Extracted nonce: ", nonce)

# Load existing pagination data if available
rds_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
if (file.exists(rds_file)) {
  message("Loading existing pagination data from ", rds_file)
  pagination_data <- readRDS(rds_file)
} else {
  pagination_data <- tibble(url = character())
}

# Prepare CSV file
output_file <- file.path("press_release_data", paste0(tolower(country_name), "_", party_id, ".csv"))
if (file.exists(output_file)) {
  message("Loading existing press releases from CSV")
  results <- read_csv(output_file)
} else {
  results <- tibble(
    title = character(),
    date = as.Date(character()),
    content = character(),
    url = character(),
    party = character(),
    country = character(),
    year = integer(),
    parlgov_party_id = integer(),
    date_collected = as.Date(character())
  )
}

# Fetch and process each page
offset <- 0
consecutive_empty_pages <- 0
max_empty_pages <- 3  # Stop after 3 consecutive pages with no links

while (consecutive_empty_pages < max_empty_pages) {
  message("Processing offset ", offset)
  
  links <- fetch_pagination_data(ajax_url, offset, nonce)
  
  if (length(links) > 0) {
    consecutive_empty_pages <- 0  # Reset counter when links are found
    new_links <- setdiff(links, pagination_data$url)
    
    if (length(new_links) > 0) {
      pagination_data <- bind_rows(pagination_data, tibble(url = new_links))
      
      for (url in new_links) {
        message("Processing URL: ", url)
        
        detailed_content <- scrape_detailed_content(url)
        
        # Add detailed content to results
        new_row <- tibble(
          title = detailed_content$title,
          date = detailed_content$date,
          content = detailed_content$content,
          url = url,
          party = party_name,
          country = country_name,
          year = year(detailed_content$date),
          parlgov_party_id = parlgov_party_id,
          date_collected = Sys.Date()
        )
        
        results <- bind_rows(results, new_row)
        
        # Save updated data to CSV
        write_csv(results, output_file)
        message("Updated and saved press release to CSV")
      }
    }
  } else {
    consecutive_empty_pages <- consecutive_empty_pages + 1
    message("No links found at offset ", offset, 
            ". Empty pages count: ", consecutive_empty_pages)
  }
  
  Sys.sleep(2)
  offset <- offset + 1000  # Increment offset by the number of items per page
}

# Save pagination data to RDS
saveRDS(pagination_data, rds_file)
message("Saved pagination data to ", rds_file) 