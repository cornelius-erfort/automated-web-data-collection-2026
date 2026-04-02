# Exercise 6: Hands-on Project
# Goal: Scrape a website of your choice, store the data, and implement best practices.
# Have a look at the examples in the course repository.

# 1. Choose a website to scrape (static or dynamic content)
# Example: urls <- c("https://example.com/page1", "https://example.com/page2")

# 2. Before you start coding, think about the neccessary steps and the data structure you want to end up with.

# 2. Set up a scraping loop
# log <- tibble::tibble(url = character(), file = character(), date = as.POSIXct(character()), status = character())

# 3. Implement feedback in the loop (print/cat progress)
# for (i in seq_along(urls)) {
#   url <- urls[i]
#   cat(sprintf("[%d/%d] Downloading: %s\n", i, length(urls), url))
#   filename <- sprintf("data/html/page_%03d_%s.html", i, format(Sys.Date(), "%Y%m%d"))
#   result <- tryCatch({
#     html <- xml2::read_html(url)
#     xml2::write_html(html, filename)
#     log <- tibble::add_row(log, url = url, file = filename, date = Sys.time(), status = "success")
#   }, error = function(e) {
#     cat(sprintf("Error downloading %s: %s\n", url, e$message))
#     log <- tibble::add_row(log, url = url, file = filename, date = Sys.time(), status = "error")
#   })
# }

# 4. Save raw HTMLs or data to files (use unique filenames)
# 5. Log URLs, download dates, and status (success/error)
# 6. Use tryCatch to handle errors and keep the script running
# 7. Use list.files() and file.exists() to avoid re-downloading

# ---
# Try to:
# - Resume failed downloads by checking which files already exist
# - Organize your files in folders (e.g., data/html/)
# - Summarize your log at the end (how many successes/errors) 