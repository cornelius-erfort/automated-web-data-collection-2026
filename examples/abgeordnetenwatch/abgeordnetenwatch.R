# Load required libraries
library(dplyr)      # For data manipulation
library(rvest)      # For web scraping
library(httr)       # For HTTP requests
library(stringr)    # For string operations
library(lubridate)  # For date and time handling

# Create a directory to store downloaded HTML pages (if it doesn't exist)
if(!dir.exists("awatch_pages")) dir.create("awatch_pages")

# Initialize a progress bar for feedback during downloads
pbar <- txtProgressBar(style = 3)

# Download all overview pages containing lists of questions and answers
for (page_number in qa_page_numbers) {
  setTxtProgressBar(pbar, length(list.files("awatch_pages"))/length(qa_page_numbers))
  
  filename <- str_c("awatch_pages/", page_number, ".html")
  
  # Skip download if file already exists
  if(file.exists(filename)) next
  
  # Construct the URL and download the page
  str_c("https://www.abgeordnetenwatch.de/bundestag/wahl-2021/fragen-antworten?page=", page_number) %>% 
    download.file(filename, quiet = T)
  
}

# Create a directory to store individual question/answer pages
if(!dir.exists("awatch_qa")) dir.create("awatch_qa")

# Initialize a vector to store all question/answer URLs
qa_links <- c()

# Extract all question/answer URLs from the overview pages
for (page_file in list.files("awatch_pages", full.names = T)) {
  
  setTxtProgressBar(pbar, (length(qa_links)/6)/length(list.files("awatch_pages")))

  # Find all links to individual Q&A pages
  qa_links <- read_html(page_file) %>% html_nodes(".tile__question__teaser a") %>% html_attr("href") %>% 
    str_c("https://www.abgeordnetenwatch.de", .) %>% c(qa_links, .)

}

# Check how many Q&A pages still need to be downloaded
qa_links[!file.exists(str_c("awatch_qa/", str_extract(qa_links, "(?<=profile/).*") , ".html") %>% str_remove("/fragen-antworten"))] %>% length

# Download all individual Q&A pages (if not already downloaded)
for (qa_link in qa_links[!file.exists(str_c("awatch_qa/", str_extract(qa_links, "(?<=profile/).*") , ".html") %>% str_remove("/fragen-antworten"))]) {
  # setTxtProgressBar(pbar,   (length(list.files("awatch_qa", recursive = T)))/length(qa_links))
  cat(".")
  (profile_name <- str_c("awatch_qa/", str_extract(qa_link, "(?<=profile/).*?(?=/)")))
  if(!dir.exists(profile_name)) dir.create(profile_name)
  
  (filename <- str_c("awatch_qa/", str_extract(qa_link, "(?<=profile/).*") , ".html") %>% str_remove("/fragen-antworten"))
    
  filename <- filename %>% str_split("(?<=/.{1,100}/)", 2)
  filename <- str_c(filename[[1]][1], filename[[1]][2] %>% str_replace_all("/", "_"))
  
  # Skip if file already exists
  if(file.exists(filename)) next

  # Download the Q&A page
  str_c(qa_link) %>% download.file(filename, quiet = T)
    
}

# Check again how many Q&A pages are missing
qa_links[!file.exists(str_c("awatch_qa/", str_extract(qa_links, "(?<=profile/).*") , ".html") %>% str_remove("/fragen-antworten"))] %>% length

# Total number of Q&A links found
qa_links %>% length

# Note: About 5% of questions are 404 (not found)

# --- Data Extraction from Q&A Pages ---
# We want to extract:
# RECEIVER: candidate name, profile url, party, track record, datetime, question text
# SENDER: citizen name, datetime, question text, issue

qa_data <- data.frame()  # Data frame to store all extracted information

total <- length(list.files("awatch_qa", full.names = T, recursive = T))
qa_files <- list.files("awatch_qa", full.names = T, recursive = T)
qa_files <- qa_files[!(qa_files %in% qa_data$filename)]

pbar <- txtProgressBar(style = 3)

# Loop through all downloaded Q&A files and extract relevant information
for (qa_file in qa_files) {
  setTxtProgressBar(pbar, (total-length(qa_files))/total)
  
  # Read the HTML file
  thispage <- read_html(qa_file)
  
  # Remove file if it's empty (download error)
  if(file.size(qa_file) == 0) {
    file.remove(qa_file)
    next
    }
  
  # Extract question text (try two possible selectors)
  (text <- thispage %>% html_node(".tile__question__text") %>% html_text)
  if(is.na(text)) (text <- thispage %>% html_node(".tile__question-text") %>% html_text)
  
  # Extract datetimes (question and answer)
  datetime <- thispage %>% html_nodes("span")
  datetime <- datetime[html_attr(datetime, "itemprop") == "datePublished" & !is.na(html_attr(datetime, "itemprop"))] %>% html_attr("content") %>% as_datetime()
  
  # Build a data frame row for this Q&A
  qa_data <- data.frame(
    filename = qa_file,
    
    # Candidate name
    cand_name = thispage %>% html_node(".tile__politician__name") %>% html_text %>% str_trim("both"),
    
    # Party affiliation
    party = thispage %>% html_node(".party-indicator") %>% html_text %>% str_trim("both"),
    
    # Teaser (short summary)
    teaser = thispage %>% html_node(".tile__question__teaser") %>% html_text %>% str_trim("both"),
    
    # Full question text
    text = text,
    
    # Answer text
    answer = thispage %>% html_node(".question-answer__text") %>% html_text,
    
    # Datetime of question and answer
    datetime_q = datetime[1],
    datetime_a = datetime[2],
    
    # Issue (topic of the question)
    issue = (thispage %>% html_nodes(".question .pile") %>% html_text)[2],
    
    # Citizen (asker) name
    citizen = thispage %>% html_node(".tile__politician__label") %>% html_text %>% str_remove("^Frage von ") %>% str_remove(" • .*")
    
  ) %>% rbind(qa_data, .)
  qa_files <- qa_files[qa_files != qa_file]
  
}

# Save the extracted data for later use
save(qa_data, file = "qa_df.RData")
load("qa_df.RData")

# Calculate response time in hours and add as a new column
qa_data$response_time_hours <- as.numeric(qa_data$datetime_a - qa_data$datetime_q, unit = "hours") %>% round(2)

# Show summary statistics of response times
qa_data$response_time_hours %>% summary

# Example: convert 85 hours to days
85/24

# Plot the distribution of response times (for responses under 300 hours)
qa_data$response_time_hours[qa_data$response_time_hours < 300] %>% density(na.rm = T) %>% plot

# Sort the data frame by candidate name and question datetime
qa_data <- qa_data[order(qa_data$cand_name, qa_data$datetime_q), ]

# Load candidate Facebook data (assumed to be pre-downloaded)
load("candidates_fb.RData")

# Check which candidates in Facebook data are also in the Q&A data
# This helps to match social media profiles with Q&A activity

# Table of matches
table(unique(candidates_fb$name) %in% unique(qa_data$cand_name))

# Proportion of Facebook candidates found in Q&A data

# Calculate the proportion
table(unique(candidates_fb$name) %in% unique(qa_data$cand_name)) / length(unique(candidates_fb$name))

# List unique candidate names from Facebook data
unique(candidates_fb$name)

# --- Merge Facebook Page IDs into Q&A Data ---
# Add page id to qa_data for easier matching with Facebook data
merge_cand <- candidates_fb[!duplicated(candidates_fb$name, candidates_fb$`Page ID`), c("name", "Page ID")]

qa_data <- merge(qa_data, merge_cand, by.x = "cand_name", by.y = "name", all.x = T)

# List all candidate names from Facebook data
candidates_fb$name

