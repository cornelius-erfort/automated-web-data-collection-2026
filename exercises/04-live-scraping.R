## Exercise 4: Live scraping with rvest + chromote
##
## Goal: extract data from a JavaScript-rendered website that `read_html()` can’t see.
##
## Tip: `read_html_live()` opens a real Chrome session in the background.
## If a cookie banner blocks the page, use `live$view()` to interact manually.

library(rvest)
library(dplyr)

# 1) Dynamic page example (CDU/CSU press releases)
url <- "https://www.cducsu.de/presse"

# 2) Load the page in a live browser session (Chrome).
live <- read_html_live(url)

# 3) Accept the cookie banner (Klaro)
# (If this fails, fall back to `live$view()` and click manually.)
live$click("button.cm-btn.cm-btn-success")

# 4) Click “Mehr laden” (loads more press releases)
# The button is a link with `rel="next"`.
for (i in 1:3) {
  live$scroll_into_view("a.button[rel='next']")
  live$click("a.button[rel='next']")
  Sys.sleep(2)
}

# 5) Extract press release items into a table
items <- live |>
  html_elements(".press-release-teasers [data-drupal-views-infinite-scroll-content-wrapper] article")

press <- tibble(
  title  = items |> html_element("a") |> html_text2(),
  url    = items |> html_element("a") |> html_attr("href"),
  date   = items |> html_element("time") |> html_text2(),
  author = items |> html_element(".truncate") |> html_text2()
)

# 6) Inspect the result
print(dplyr::slice_head(press, n = 10))

# 7) Save as CSV (optional)
# write.csv(press, "data/cducsu_press.csv", row.names = FALSE)
