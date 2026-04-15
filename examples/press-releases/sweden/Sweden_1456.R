library(httr)
library(jsonlite)
library(tidyverse)
library(xml2)
library(rvest)

# Set Sverigedemokraterna specific variables
api_url <- "https://via.tt.se/public-website-api/pressroom/3236128/releases/5000/0"
party_id <- 1456
country_name <- "Sweden"
party_name <- "Sverigedemokraterna"
parlgov_party_id <- 1456

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to fetch and save pagination data
fetch_pagination_data <- function(api_url) {
  tryCatch({
    response <- GET(api_url)
    
    if (status_code(response) != 200) {
      stop("Failed to fetch data, status code: ", status_code(response))
    }
    
    data <- fromJSON(rawToChar(response$content))
    releases <- data$releases
    
    # Extract relevant fields
    pagination_data <- tibble(
      # date = as.POSIXct(sapply(releases, function(x) x$date), format = "%Y-%m-%dT%H:%M:%S", tz = "GMT") %>% as.Date,
      # title = sapply(releases, function(x) x$versions$sv$title),
      # content = sapply(releases, function(x) x$versions$sv$metadescription),
      url = paste0("https://via.tt.se", releases$versions$sv$url)
    )
    
    # Save pagination data to RDS
    rds_file <- file.path("meta-data", paste0("pagination_", party_id, ".rds"))
    saveRDS(pagination_data, rds_file)
    
    message("Saved pagination data to ", rds_file)
    
    return(pagination_data)
    
  }, error = function(e) {
    message("Error fetching pagination data: ", e$message)
    return(NULL)
  })
}

# Function to scrape detailed content from individual pages
scrape_detailed_content <- function(url) {
  tryCatch({
    page <- read_html(url)
    title <- page %>% html_node(".text-elements__ReleaseTitle-sc-1il5uxg-1") %>% html_text(trim = TRUE)
    date <- page %>% html_node(".release__Byline-sc-6son67-1") %>% html_text(trim = TRUE)
    content <- page %>% html_node(".release__PublicationContent-sc-6son67-0") %>% html_text(trim = TRUE)
    data.frame(title = title, date = date, content = content)
  }, error = function(e) {
    message("Error scraping detailed content from ", url, ": ", e$message)
    return(data.frame(title = NA, date = NA, content = NA))
  })
}

# Main execution
message("Starting Sverigedemokraterna press release scraper...")
pagination_data <- fetch_pagination_data(api_url)

if (!is.null(pagination_data)) {
  # Prepare CSV file
  output_file <- file.path("press_release_data", paste0(tolower(country_name), "_", party_id, ".csv"))
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
  
  for (i in seq_len(nrow(pagination_data))) {
    url <- pagination_data$url[i]
    message("Processing URL: ", url)
    
    detailed_content <- scrape_detailed_content(url)
    
    # Add detailed content to results
    new_row <- tibble(
      title = detailed_content$title,
      date = dmy(detailed_content$date %>% str_extract("\\d{1,4}\\.\\d{1,2}\\.\\d{1,4}")),
      content = detailed_content$content,
      url = url,
      party = party_name,
      country = country_name,
      year = as.integer(format(dmy(detailed_content$date %>% str_extract("\\d{1,4}\\.\\d{1,2}\\.\\d{1,4}")), "%Y")),
      parlgov_party_id = parlgov_party_id,
      date_collected = Sys.Date()
    )
    
    results <- bind_rows(results, new_row)
    
    # Save updated data to CSV
    write_csv(results, output_file)
    message("Updated and saved press release ", i, " to CSV")
  }
} 