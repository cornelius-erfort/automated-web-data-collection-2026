# Exercise 5: httr (Solution)
# Demonstrate GET/POST requests, headers, cookies, and response inspection

library(httr)
library(rvest)
library(xml2)
library(stringr)

# 1. Send a simple GET request to https://www.reddit.com/r/sociology/, i.e. GET(url) (or any other reddit page with several posts)
resp <- GET("https://www.reddit.com/r/sociology/")

# 2. Inspect the response, e.g. status code, headers, content
print(status_code(resp))
print(headers(resp))
content(resp)

# 3. Extract the HTML content
html_content <- content(resp)

# 4. Extract the titles of the posts using a css selector
html_posts <- html_nodes(html_content, ".absolute")

# 5. Extract the text of the posts (html_text)
html_text(html_posts)

# 6. Extract the links of the posts (html_attr)
html_attr(html_posts, "href")

# 7. (optional) Get the author names of the posts
html_text(html_nodes(html_content, "faceplate-tracker[source='post_credit_bar']"))

# 8. Inspect the network traffic using a dev tools in a browser. How are subsequent posts loaded when scrolling down?
# https://www.reddit.com/svc/shreddit/community-more-posts/best/?after=dDNfMWs5NnN4Yg==&t=DAY&name=sociology&ad_posts_served=8&navigationSessionId=b3d4997a-f508-4fbf-a75e-4c61ac17e3ea&feedLength=61&distance=57&adDistance=4
str_split("after=dDNfMWs5NnN4Yg==&t=DAY&name=sociology&ad_posts_served=8&navigationSessionId=b3d4997a-f508-4fbf-a75e-4c61ac17e3ea&feedLength=61&distance=57&adDistance=4", "&")

# 9. Inspect the GET request. What could the fields mean?

# 10. Try replicating the GET request using httr. 
resp <- GET("https://www.reddit.com/svc/shreddit/community-more-posts/best/?after=dDNfMWs5NnN4Yg==&t=DAY&name=sociology&ad_posts_served=8&navigationSessionId=b3d4997a-f508-4fbf-a75e-4c61ac17e3ea&feedLength=61&distance=57&adDistance=4")
resp

# 11. To load the next set of posts, we need to modify the request. The URL to the next set can be found in the response content from the previous.


# 12. Extract the url for the next page
next_url <- html_content %>%
  html_node('faceplate-partial[slot="load-after"]') %>%
  html_attr("src")

# 13. Build the full URL for the next request by adding "https://www.reddit.com"
next_url <- paste0("https://www.reddit.com", next_url)

# 14. Request the next page
resp2 <- GET(next_url)
# ...repeat as needed, extracting the next url each time

# 15. Extract the posts from this page
html_content2 <- content(resp2)

html_posts2 <- html_nodes(html_content2, ".absolute")
html_text(html_posts2)
html_attr(html_posts2, "href")

# 16. Build a loop to extract the posts from the next pages and store titles and links in a data frame.
sociology_posts <- data.frame(title = character(), link = character())

for (i in 1:50) {
    print(i)
  resp <- GET(next_url)
  html_content <- content(resp)
  html_posts <- html_nodes(html_content, ".absolute")
  titles <- html_text(html_posts)
  links <- html_attr(html_posts, "href")
  sociology_posts <- rbind(sociology_posts, data.frame(title = titles, link = links))
  next_url <- html_content %>%
    html_node('faceplate-partial[slot="load-after"]') %>%
    html_attr("src")
  next_url <- paste0("https://www.reddit.com", next_url)
  if (is.null(next_url)) break
}


sociology_posts$title <- str_trim(sociology_posts$title)
sociology_posts <- sociology_posts[sociology_posts$title != "",]

View(sociology_posts)
