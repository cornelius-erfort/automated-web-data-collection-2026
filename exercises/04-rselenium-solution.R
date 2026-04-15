# Exercise 4: RSelenium
# Goal: Use RSelenium to automate a browser, interact with a page, and extract content after JS loads.


# Install chromdriver (Chrome) or geckodriver (Firefox)
# https://github.com/ropensci/RSelenium/issues/100

# 1. Load the RSelenium package
library(RSelenium)

# 2. Start Selenium server and browser
rD <- rsDriver(browser = "chrome", port = 4555L)
remDr <- rD$client

# remDr$

# 3. Navigate to Google
remDr$navigate("https://www.google.com")

# 4. Find the search box (by name attribute 'q')
search_box <- remDr$findElement(using = "name", value = "q")

# 5. Enter your search term (e.g., "web scraping in R")
search_box$sendKeysToElement(list("web scraping in R", key = "enter"))

# 6. Wait for results to load (optional: Sys.sleep(2) or use explicit waits)
Sys.sleep(2)

# 7. Extract the titles of the search results (as an example)
results <- remDr$findElements(using = "css selector", value = "h3")


remDr$getPageSource()[[1]]

titles <- sapply(results, function(x) x$getElementText())
print(titles)

# 8. Close browser and stop server
remDr$close()
rD$server$stop()

remDr$navigate("https://www.reddit.com/r/sociology/")

# Scroll down
for (i in 1:3) {
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(2)
}


remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")

remDr$getPageSource()[[1]]


# ---
# Try to:
# - Find other dynamic websites to scrape
# - Interact with different elements (links, forms, etc.)
# - Extract and print specific content from the loaded page 