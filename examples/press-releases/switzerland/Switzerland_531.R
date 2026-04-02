


# Set Christlichdemokratische Volkspartei specific variables
base_url <- "https://die-mitte.ch/aktuelles/communiques/"
party_id <- 531
country_name <- "Switzerland"
party_name <- "Christlichdemokratische Volkspartei"
parlgov_party_id <- 531

# Function to extract press release links from a page
extract_press_links <- function(page_url) {
  tryCatch({
    page <- read_html(page_url)
    links <- page %>%
      html_nodes(".card--v2") %>%
      html_attr("href") %>%
      unique()
    
    return(links)
  }, error = function(e) {
    warning("Error extracting links from ", page_url, ": ", e$message)
    return(character())
  })
}

# Function to fetch and process data from the main communique page
fetch_and_process_data <- function(base_url) {
  message("Processing main communique page")
  
  links <- extract_press_links(base_url)
  
  if (length(links) > 0) {
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
    
    for (url in links) {
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
      output_file <- file.path("press_release_data", paste0(tolower(country_name), "_", party_id, ".csv"))
      write_csv(results, output_file)
      message("Updated and saved press release to CSV")
    }
  } else {
    message("No links found on the main communique page")
  }
  
  message("Completed processing the main communique page.")
}

# Function to translate Swiss dates to English format
parse_swiss_date <- function(swiss_date) {
  month_dict <- c(
    "Januar" = "January", "Februar" = "February", "März" = "March", 
    "April" = "April", "Mai" = "May", "Juni" = "June", 
    "Juli" = "July", "August" = "August", "September" = "September", 
    "Oktober" = "October", "November" = "November", "Dezember" = "December"
  )
  
  for (german_month in names(month_dict)) {
    english_month <- month_dict[[german_month]]
    swiss_date <- str_replace(swiss_date, german_month, english_month)
  }
  
  swiss_date <- str_extract(swiss_date, "\\d+\\.\\s\\w+\\s\\d{4}")
  
  parsed_date <- dmy(swiss_date)
  return(parsed_date)
}

# Function to scrape detailed content from individual pages
scrape_detailed_content <- function(url) {
  tryCatch({
    page <- read_html(url)
    (title <- page %>% html_node("h1") %>% html_text(trim = TRUE))
    (date_french <- page %>% html_node(".subtitle") %>% html_text(trim = TRUE))
    (date <- parse_swiss_date(date_french))
    (content <- page %>% html_nodes(".content > :not(.breadcrumb):not(h1)") %>% html_text(trim = TRUE) %>% str_c(collapse = "\n"))
    list(title = title, date = date, content = content)
  }, error = function(e) {
    message("Error scraping detailed content from ", url, ": ", e$message)
    return(list(title = NA, date = NA, content = NA))
  })
}

# Main execution
message("Starting Christlichdemokratische Volkspartei press release scraper...")
fetch_and_process_data(base_url)
