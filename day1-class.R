# Download the page that lists recent press releases
myhtml <- read_html("https://labour.org.uk/category/latest/press-release/page/1")

# Select all relevant link elements with a CSS selector
myelements <- html_elements(myhtml, ".post-preview-compact__link")

# Extract the href attribute (relative URLs)
links <- html_attr(myelements, "href")

# Turn relative paths into absolute URLs
links <- str_c("https://labour.org.uk", links)

links

# Check the first few collected links
head(links, 5)


# Create a local folder once to store downloaded pages
dir.exists("labour")

dir.create("labour")

dir.exists("labour")

basename(links[1])

for (link in links) {

  filename <- str_c("labour/", basename(link), ".html")
  
  if(file.exists(filename)) next
  
  myhtml <- read_html(link)
  
  write_html(myhtml, file = filename)
  
  Sys.sleep(1)
  
}


pages <- str_c("https://labour.org.uk/updates/press-releases/page/", 1:9, "/")

page <- pages[1]

for (page in pages) {
  
  filename <- str_c("labour/", basename(page), ".html")
  
  if(file.exists(filename)) next
  
  myhtml <- read_html(link)
  
  write_html(myhtml, file = filename)
  
  Sys.sleep(1)
  
}





