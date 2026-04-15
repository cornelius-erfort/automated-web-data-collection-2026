library(tidyverse)
library(httr)
library(rvest)
library(stringr)
library(here)
library(RSelenium)
library(binman)


# Function to install specific ChromeDriver version
install_chromedriver <- function(version = "136.0.7103.92") {
  # 1. Get binman directory
  dir_path <- binman::app_dir("chromedriver", check = TRUE)
  dir_path <- str_c(dir_path, "/mac64_m1", sep = "")
  message("ChromeDriver directory: ", dir_path)
  
  # 2. Use direct Chrome for Testing download URL
  download_url <- sprintf("https://storage.googleapis.com/chrome-for-testing-public/%s/mac-arm64/chromedriver-mac-arm64.zip", version)
  message("Download URL: ", download_url)
  
  # 3. Execute commands one by one with proper path handling
  # Create version directory
  version_dir <- file.path(dir_path, version)
  dir.create(version_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Change to the version directory
  setwd(version_dir)
  
  # Download ChromeDriver
  download.file(download_url, "chromedriver_mac_arm64.zip", mode = "wb")
  
  # Unzip the file
  unzip("chromedriver_mac_arm64.zip")
  
  # The chromedriver is in a subdirectory called 'chromedriver-mac-arm64'
  file.copy("chromedriver-mac-arm64/chromedriver", "chromedriver", overwrite = TRUE)
  
  # Remove the subdirectory
  unlink("chromedriver-mac-arm64", recursive = TRUE)
  
  # Make the chromedriver executable
  system("chmod +x chromedriver")
  
  # Clean up zip file
  unlink("chromedriver_mac_arm64.zip")
  
  # 4. Verify installation
  versions <- binman::list_versions("chromedriver")
  message("Available ChromeDriver versions: ")
  print(versions)
  
  # Reset working directory
  setwd(getwd())
}

# Install the specific version
install_chromedriver("136.0.7103.92")




################################


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
  chromever = "136.0.7103.92",
  extraCapabilities = chrome_options
)

remote_driver <- driver$client

# remote_driver$navigate("https://d66.nl/nieuws/")

remote_driver$navigate("https://chatgpt.com")

# NRW Parliament Plenary Protocols Scraping

remote_driver$navigate("https://www.landtag.nrw.de/home/dokumente/dokumentensuche/ubersichtsseite-reden--protoko-1/protokolle-18-wahlperiode-2022-2.html?ausschuss=Plenum&page=1")

# Find all elements matching the CSS selector
elements <- remote_driver$findElements(using = "css selector", ".e-event-timeline__link:nth-child(3) a")
  
# Extract href attributes
pdf_links <- sapply(elements, function(el) el$getElementAttribute("href")[[1]])

pdf_links[1] %>% remote_driver$navigate()


if (exists("remote_driver")) remote_driver$close()
if (exists("driver")) driver$server$stop()

    
