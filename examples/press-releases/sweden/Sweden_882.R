library(httr)
library(jsonlite)
library(tidyverse)
library(xml2)

# Set Vänsterpartiet specific variables
api_url <- "https://www.vansterpartiet.se/page-data/sq/d/559947168.json"
party_id <- 882
country_name <- "Sweden"
party_name <- "Vänsterpartiet"
parlgov_party_id <- 882

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Function to fetch and process press releases
fetch_vansterpartiet_press_releases <- function(api_url) {
  tryCatch({
    response <- GET(api_url)
    
    if (status_code(response) != 200) {
      stop("Failed to fetch data, status code: ", status_code(response))
    }
    
    data <- fromJSON(rawToChar(response$content))
    nodes <- data$data$graphQlQuery$data$contentNodes$nodes
    
    # Extract relevant fields and clean HTML tags
    press_releases <- tibble(
      date = as.POSIXct(nodes$dateGmt, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT") %>% as.Date,
      title = nodes$title,
      content = sapply(nodes$content, function (x) {
        # Try to parse the content as HTML
        tryCatch({
          html_text(read_html(x))
        }, error = function(e) {
          x  # Return the original content if it's not HTML
        })
      }),
      url = paste0("https://www.vansterpartiet.se", nodes$uri),
      party = party_name,
      country = country_name,
      year = as.integer(format(as.POSIXct(nodes$dateGmt, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"), "%Y")),
      parlgov_party_id = parlgov_party_id,
      date_collected = Sys.Date()
    )
    
    # Save to CSV
    output_file <- file.path("press_release_data", paste0(tolower(country_name), "_", party_id, ".csv"))
    write_csv(press_releases, output_file)
    
    message("Saved ", nrow(press_releases), " press releases to ", output_file)
    
    return(press_releases)
    
  }, error = function(e) {
    message("Error fetching press releases: ", e$message)
    return(NULL)
  })
}

# Main execution
message("Starting Vänsterpartiet press release scraper...")
press_releases <- fetch_vansterpartiet_press_releases(api_url) 

