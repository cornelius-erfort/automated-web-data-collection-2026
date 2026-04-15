library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(stringr)
library(lubridate)
library(here)
library(RSelenium)
library(binman)

# Set D66 specific variables
party_url <- "https://d66.nl/wp-json/d66/v1/article-listing"
party_id <- 345
country_name <- "Netherlands"

# Create directories if they don't exist
if (!dir.exists("meta-data")) dir.create("meta-data")
if (!dir.exists("press_release_data")) dir.create("press_release_data")

# Dutch month conversion lookup (keeping this for processing dates in press releases)
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


# Function to extract press release links using the driver
extract_d66_press_links <- function(remote_driver) {
  # Save current working directory
  
  tryCatch({
    # Set working directory to project root
    setwd(here())
    
    # First get the nonce from the news page
    remote_driver$navigate("https://d66.nl/nieuws/")
    
    # Wait for manual Cloudflare verification if needed
    message("Please complete Cloudflare verification if needed...")
    message("Press Enter once the page is loaded...")
    readline("Press Enter to continue...")
    
    # Get the nonce
    nonce <- remote_driver$findElement(using = "css", "#_wpnonce")$getElementAttribute("value")[[1]]
    message("Got nonce: ", nonce)
    
    # Construct the JSON API URL
    json_url <- sprintf("https://d66.nl/wp-json/d66/v1/article-listing?_wpnonce=%s&howManyToLoad=2400&postType=post&firstPostId=0&showLoadMoreButton=true&loadMoreButtonText=%%0A%%09%%0A%%09%%09%%0A%%09%%09%%0A%%09%%09%%09Meer+Laden%%09%%09%%0A%%0A%%09%%09%%09%%0A%%0A%%09%%0A%%09%%09%%09%%0A%%0A%%09%%0A&buttonAdditionalClasses=js-article-listing-load-more&buttonParentClass=block-article-listing&isEverythingAccordion=true&postFiltering=no-filter&selectedPostCategories%%5B%%5D=126&selectedPostCategories%%5B%%5D=127&selectedPostCategories%%5B%%5D=125&selectedPostCategories%%5B%%5D=128&selectedPostCategories%%5B%%5D=1&selectedSearchTags=undefined&selectedPeople=undefined&showNationalContent=undefined&showRelatedContent=undefined&syndication=current-site&lastSiteId=1&postParentId=&paginationAdditionalClass=js-article-listing-pagination&paginationCurrentPage=1&showPaginationText=true", nonce)
    
    # Navigate to the JSON URL
    remote_driver$navigate(json_url)
    
    # Get the JSON response from the page
    json_text <- remote_driver$getPageSource()[[1]]
    
    # Parse the JSON
    json_data <- fromJSON(json_text %>% read_html %>% html_text)
    
    # Extract links from HTML content in JSON
    html_content <- read_html(json_data$data$html)
    links <- html_content %>%
      html_nodes("a") %>%
      html_attr("href") %>% 
      unique
    
    # Return unique links
    unique(links)
    
  }, error = function(e) {
    message("Error extracting links: ", e$message)
    return(character())
  })
}

# Function to process a single press release with ChromeDriver
process_d66_press_release <- function(url, remote_driver) {
  tryCatch({
    remote_driver$navigate(url)
    Sys.sleep(2)
    
    # Extract title with error handling
    title <- tryCatch({
      title_elem <- remote_driver$findElement(using = "css", ".heading__size--giant")
      str_c(title_elem$getElementText(), collapse = " ")
    }, error = function(e) {
      warning(sprintf("Could not find title for %s: %s", url, e$message))
      return(NA_character_)
    })
    
    # Extract date with error handling
    date <- tryCatch({
      date_elem <- remote_driver$findElement(using = "css", ".block-related-post-metadata__date")
      date_text <- date_elem$getElementText()[[1]]
      as.Date(gsub("^\\s+|\\s+$", "", strsplit(date_text, "\n")[[1]][1]), format = "%d.%m.%Y")
    }, error = function(e) {
      # Try to get date from JSON-LD schema
      tryCatch({
        schema_elem <- remote_driver$findElement(using = "css", "script[type='application/ld+json'].yoast-schema-graph")
        schema_text <- schema_elem$getElementAttribute("innerHTML")[[1]]
        schema_data <- fromJSON(schema_text)
        min(schema_data$`@graph`$datePublished, na.rm = T) %>% as.Date
        
      }, error = function(e) {
        warning(sprintf("Could not find date in schema for %s: %s", url, e$message))
        return(NA)
      })
    })
    
    # Only proceed if we have at least a title
    if (is.na(title)) {
      warning(sprintf("Skipping %s due to missing title", url))
      return(NULL)
    }
    
    # Extract text content
    text_parts <- c()
    
    # Get subheading (if exists)
    subheading_elements <- remote_driver$findElements(using = "css", ".paragraph__size--big")
    if (length(subheading_elements) > 0) {
      subheading <- subheading_elements[[1]]$getElementText()
      text_parts <- c(text_parts, subheading)
    }
    
    # Get all text content from different paragraph types and sections
    content_selectors <- c(
      ".block-paragraph__paragraph",  # Regular paragraphs
      ".block-post-content__content .paragraph",  # Content paragraphs
      ".block-post-content .paragraph__size--default",  # Default-sized paragraphs
      ".block-infographics__content-text",  # Infographics text content
      ".block-infographics__content-title",  # Infographics titles
      ".block-video__caption .paragraph",  # Video captions
      ".image__caption .paragraph",  # Image captions
      ".block-video__video-description .paragraph",  # Video descriptions
      ".related-person-info__description .paragraph"  # Person descriptions
    )
    
    for (selector in content_selectors) {
      tryCatch({
        elements <- remote_driver$findElements(using = "css", selector)  # Note: changed content_selectors to selector
        if (length(elements) > 0) {
          text <- sapply(elements, function(x) {
            tryCatch({
              x$getElementText()[[1]]
            }, error = function(e) {
              warning(sprintf("Error getting text for selector '%s': %s", selector, e$message))
              return("")
            })
          })
          
          # Filter out empty strings and add non-empty text to text_parts
          valid_text <- text[nchar(text) > 0]
          if (length(valid_text) > 0) {
            text_parts <- c(text_parts, valid_text)
          }
        }
      }, error = function(e) {
        warning(sprintf("Error processing selector '%s': %s", selector, e$message))
      })
    }
    
    text_parts <- unique(text_parts)
    
    # Combine all text parts with proper spacing
    full_text <- paste(text_parts, collapse = "\n\n")
    
    # Final cleanup
    full_text <- gsub("\n{3,}", "\n\n", full_text)  # Normalize multiple newlines
    full_text <- gsub(" {2,}", " ", full_text)  # Remove multiple spaces
    full_text <- gsub("^\n+|\n+$", "", full_text)  # Trim leading/trailing newlines
    full_text <- trimws(full_text)  # Final trim
    
    # Return as tibble row
    tibble(
      title = title,
      date = date,
      text = full_text,
      url = url
    )
    
  }, error = function(e) {
    message(sprintf("Error processing %s: %s", url, e$message))
    return(NULL)
  })
}

# Main function to extract and process press releases
extract_d66_press_releases <- function() {
  original_wd <- getwd()
  
  tryCatch({
    setwd(here())
    
    # Set up output file path
    output_file <- file.path("press_release_data", "netherlands_345.csv")
    
    # load remove na dates and save
    # data <- read_csv("press_release_data/netherlands_345.csv")
    # data <- data %>% filter(!is.na(date))
    # write_csv(data, "press_release_data/netherlands_345.csv")
    
    
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
    
    message("Starting Chrome driver...")
    chrome_options <- list(
      chromeOptions = list(
        args = c(
          '--no-sandbox',
          '--start-maximized',
          '--disable-blink-features=AutomationControlled',
          '--disable-dev-shm-usage'
        ),
        excludeSwitches = list('enable-automation'),
        useAutomationExtension = FALSE
      )
    )
    
    driver <- rsDriver(
      browser = "chrome",
      port = 4447L,
      chromever = "134.0.6998.165",
      extraCapabilities = chrome_options
    )
    
    remote_driver <- driver$client
    
    # Get all press release links
    urls <- extract_d66_press_links(remote_driver)
    message(sprintf("Found %d press release links", length(urls)))
    
    # Filter out already processed URLs
    new_urls <- setdiff(urls, existing_urls)
    message(sprintf("Found %d new press releases to process", length(new_urls)))
    
    # Process each press release
    for(url in new_urls) {
      message(sprintf("Processing press release: %s", url))
      result <- process_d66_press_release(url, remote_driver)
      
      if(!is.null(result)) {
        # Add additional columns
        result <- result %>%
          mutate(
            party = "D66",
            country = "Netherlands",
            year = year(date),
            parlgov_party_id = 345,
            date_collected = Sys.Date()
          )
        
        # Append to CSV
        write_csv(result, output_file, append = TRUE)
        message(sprintf("Saved press release: %s", result$title))
      }
      
      Sys.sleep(1)  # Rate limiting
    }
    
    # Read and return final results
    final_results <- read_csv(output_file, show_col_types = FALSE)
    message(sprintf("Total press releases in file: %d", nrow(final_results)))
    
    return(final_results)
    
  }, error = function(e) {
    message("Error occurred: ", e$message)
    return(tibble())
  }, finally = {
    if (exists("remote_driver")) remote_driver$close()
    if (exists("driver")) driver$server$stop()
    setwd(original_wd)
  })
}

# Run the scraper
press_releases <- extract_d66_press_releases()

# Usage:
# First time setup - install ChromeDriver:
# install_chromedriver("108.0.5359.71")

# Then run the scraper:
# extract_d66_press_links() 
