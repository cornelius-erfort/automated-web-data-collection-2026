# Exercise 5: httr (Solution)
# Demonstrate GET/POST requests, headers, cookies, and response inspection


# 1. Send a simple GET request to https://www.reddit.com/r/sociology/, i.e. GET(url) (or any other reddit page with several posts)

# 2. Inspect the response, e.g. status code, headers, content

# 3. Extract the HTML content

# 4. Extract the titles of the posts using a css selector

# 5. Extract the text of the posts (html_text)

# 6. Extract the links of the posts (html_attr)

# 7. (optional) Get the author names of the posts

# 8. Inspect the network traffic using a dev tools in a browser. How are subsequent posts loaded when scrolling down?

# 9. Inspect the GET request. What could the fields mean?

# 10. Try replicating the GET request using httr. 

# 11. To load the next set of posts, we need to modify the request. The URL to the next set can be found in the response content from the previous.

# 12. Extract the url for the next page

# 13. Build the full URL for the next request by adding "https://www.reddit.com"

# 14. Request the next page

# 15. Extract the posts from this page

# 16. Build a loop to extract the posts from the next pages and store titles and links in a data frame.

