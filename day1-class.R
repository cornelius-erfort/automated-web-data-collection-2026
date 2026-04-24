
# Create a local folder once to store downloaded pages
dir.exists("labour")

dir.create("labour")

dir.exists("labour")

basename(links[1])




dir.create("labour")

pages <- str_c("https://labour.org.uk/updates/press-releases/page/", 1:9, "/")

for (page in pages) {
  
  filename <- str_c("labour/", basename(page), ".html")
  
  if(file.exists(filename)) next
  
  myhtml <- read_html(page)
  
  write_html(myhtml, file = filename)
  
  Sys.sleep(1)
  
}

all_links <- c()

for (filename in list.files("labour")) {
  
  myhtml <- read_html(str_c("labour/", filename))
  
  mylinks <- html_elements(myhtml, ".post-preview-compact__link")
  
  mylinks <- html_attr(mylinks, "href")
  
  all_links <- c(mylinks, all_links)

}

all_links



for (link in links) {
  
  filename <- str_c("labour/", basename(link), ".html")
  
  if(file.exists(filename)) next
  
  myhtml <- read_html(link)
  
  write_html(myhtml, file = filename)
  
  Sys.sleep(1)
  
}


