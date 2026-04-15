# Exercise 7: Helpful Tools for Web Scraping


# This exercise will guide you through using helpful tools in R for web scraping.
# We will cover error handling with tryCatch, logging and feedback, saving data to files,
# and checking conditions in loops. Each section includes explanations and tasks.

# ------------------------------------------------------------
# Error Handling with tryCatch
# ------------------------------------------------------------

# tryCatch is a powerful tool for handling errors gracefully in R.
# It allows your script to continue running even if an error occurs,
# and you can specify actions to take when an error is caught.

# Example:
tryCatch({
  # Code that might throw an error
  result <- read_html("https://example.com")
}, error = function(e) {
  cat("Error occurred:", e$message, "\n")
})

tryCatch({
  # Code that might throw an error
  result <- read_html("https://exapmle.com")
}, error = function(e) {
  cat("Error occurred:", e$message, "\n")
})


# Task:
# Use tryCatch to handle potential errors in a web scraping loop.
# Log errors to a file for later review.

# ------------------------------------------------------------
# Simple Feedback Mechanisms
# ------------------------------------------------------------

# Providing simple feedback during long-running scripts helps track progress and diagnose issues.
# Use cat() or print() to output messages to the console.
# This is useful for understanding the flow of your script and ensuring that it is running as expected.

# Example:
for (i in 1:10) {
  cat("Processing item", i, "\n")
  Sys.sleep(1)  # Simulate processing time
}

# ------------------------------------------------------------
# Feedback and Logging
# ------------------------------------------------------------

# Logging is crucial for tracking the progress and diagnosing issues in web scraping.
# It involves saving important information like timestamps and URLs to a log file.
# This helps in understanding the flow of the script and identifying where issues occur.
# Helpful functions for this are Sys.Date() and Sys.time().



# Example:
library(stringr)
log_file <- "scraping_log.txt"

# Function to log messages with timestamps
log_message <- function(message) {
  timestamp <- Sys.time()
  cat(str_c("[", timestamp, "] ", message, "\n"), file = log_file, append = TRUE)
}

# Simulate a scraping loop with logging
urls <- c("https://example.com/page1", "https://example.com/page2")
for (url in urls) {
  log_message(paste("Starting to scrape", url))
  
  # Simulate scraping process
  Sys.sleep(1)
  
  # Simulate saving the scraped data to a local file
  local_file <- str_c("output/", basename(url), ".html")
  # Here you would save the actual data, e.g., writeLines(html_content, local_file)
  
  log_message(paste("Finished scraping", url, "and saved to", local_file))
}

# Task:
# Implement first feedback and then logging in a scraping loop from a previous exercise.
# Record the start and end time of each URL processed and log the file path where the scraped data is saved.

# ------------------------------------------------------------
# Saving Data to Files
# ------------------------------------------------------------

# Saving data to files is crucial for preserving results and debugging.
# Use write.csv(), writeLines(), or saveRDS() to save data in R.
# Files are often saved in formats like .html or .json, depending on the data type.

# Example:
data <- data.frame(x = 1:5, y = letters[1:5])
write.csv(data, "output/data.csv", row.names = FALSE)

# Simulate saving HTML content
library(xml2)
dummy_html_content <- read_html("<html><body><h1>Example</h1></body></html>")
write_html(dummy_html_content, "example.html")

# Simulate saving JSON content
library(jsonlite)
dummy_json_content <- toJSON(list(name = "example", value = 123))
writeLines(dummy_json_content, "example.json")


# ------------------------------------------------------------
# Checking Conditions in Loops
# ------------------------------------------------------------

# Often, you need to check conditions within loops to decide whether to continue processing.
# Use if statements to check for conditions like file existence or data validity.

# Example:
files <- list.files("data")
for (file in files) {
  if (file.exists(file)) {
    cat("Processing", file, "\n")
  } else {
    cat("File not found:", file, "\n")
  }
}

# Task:
# Implement a loop that checks for the existence of files before processing them.

# ------------------------------------------------------------
# Saving Metadata
# ------------------------------------------------------------

# Similar to logging, you can save metadata such as the source URL and date-downloaded.
# This can be done by storing the information in a dataframe or incorporating it into the file name.
# Helpful functions for this are Sys.Date() and Sys.time().

# Example:
metadata <- data.frame(
  url = character(),
  date_downloaded = as.Date(character()),
  file_path = character(),
  stringsAsFactors = FALSE
)

# Simulate a scraping loop with metadata collection
urls <- c("https://example.com/page1", "https://example.com/page2")
for (url in urls) {
  date_downloaded <- Sys.Date()
  local_file <- str_c("output/", basename(url), "_", date_downloaded, ".html")
  
  # Here you would save the actual data, e.g., writeLines(html_content, local_file)
  
  # Add metadata to the dataframe
  metadata <- rbind(metadata, data.frame(url = url, date_downloaded = date_downloaded, file_path = local_file))
}

# Task:
# Implement a system to save metadata like source URL and date-downloaded in a dataframe.
# Consider incorporating this information into the file name as well.

# ------------------------------------------------------------
# Using writeLines
# ------------------------------------------------------------

# The writeLines function is used to write text data to a file.
# It is particularly useful for saving plain text, HTML, or JSON content.
# You specify the text to write and the file path where it should be saved.

# Example:
# Here we save JSON content to a file using writeLines.
# The JSON content is first converted to a string using toJSON.

library(jsonlite)
dummy_json_content <- toJSON(list(name = "example", value = 123))
writeLines(dummy_json_content, "example.json")

# Task:
# Use writeLines to save text data to a file, such as HTML or JSON content.
# Verify the contents of the file to ensure it was saved correctly.

# ------------------------------------------------------------
# Using %in% Operator
# ------------------------------------------------------------

# The %in% operator is used to check if elements of one vector are present in another vector.
# It returns a logical vector indicating if there is a match or not for each element.

# Example:
# Check if certain values are present in a vector
values <- c(1, 2, 3, 4, 5)
check_values <- c(2, 4, 6)

# Use %in% to check membership
result <- check_values %in% values

# Output the result
cat("Check values:", check_values, "\n")
cat("Are in values:", result, "\n")

# Task:
# Use the %in% operator to check if specific URLs are present in a list of scraped URLs.
# Print the results to verify the presence of each URL.

# ------------------------------------------------------------
# Stringr Functions
# ------------------------------------------------------------

# The stringr package provides a cohesive set of functions designed to make working with strings as easy as possible.
# Here are some commonly used functions:

# str_c: Concatenates strings
example_string <- str_c("Hello", "World", sep = " ")
cat("Concatenated string:", example_string, "\n")

# str_detect: Detects the presence of a pattern in a string
has_hello <- str_detect(example_string, "Hello")
cat("Contains 'Hello':", has_hello, "\n")

# str_replace: Replaces matched patterns in a string
replaced_string <- str_replace(example_string, "World", "R")
cat("Replaced string:", replaced_string, "\n")

# str_split: Splits a string into substrings
split_string <- str_split(example_string, " ")
cat("Split string:", split_string, "\n")

# str_trim: Trims whitespace from a string
trimmed_string <- str_trim("  Hello World  ")
cat("Trimmed string:", trimmed_string, "\n")

# Task:
# Use stringr functions to manipulate and analyze strings in your web scraping tasks.
# For example, use str_detect to find specific patterns in scraped text. 