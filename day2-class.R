library(rvest)
library(dplyr)

# Start a live Chrome session and load the page
live <- read_html_live("https://www.cducsu.de/presse")

live$view()

# Handle cookie banner (Klaro): click “Alle Cookies akzeptieren”
live$click("button.cm-btn.cm-btn-success")

# Click "Mehr laden" (load more items)

for (i in 1:20) {
  live$scroll_into_view("a.button[rel='next']")
  
  live$click("a.button[rel='next']")
  
  Sys.sleep(1)
}


# Extract press releases currently visible
items <- live %>%
  html_elements(".press-release-teasers [data-drupal-views-infinite-scroll-content-wrapper] article")

# items <- read_html("https://www.cducsu.de/presse")  %>%
#   html_elements(".press-release-teasers [data-drupal-views-infinite-scroll-content-wrapper] article")

items %>% length

out <- data.frame(
  title  = items %>% html_element("a") %>% html_text2(),
  url    = items %>% html_element("a") %>% html_attr("href") %>% paste0("https://www.cducsu.de", .),
  date   = items %>% html_element("time") %>% html_text2(),
  author = items %>% html_element(".truncate") %>% html_text2()
)

out %>% slice_head(n = 10)

library(jsonlite)

indian_parliament <- fromJSON("https://sansad.in/api_ls/member?loksabha=18&state=&party=&gender=&ageFrom=&ageTo=&noOfTerms=&page=1&size=1000&searchText=&constituency=&sitting=1&locale=en&month=&profession=&otherProfession=&constituencyCategory=&positionCode=&qualification=&noOfChildren=&isFreedomFighter=&memberStatus=s")

class(indian_parliament)

indian_parliament[[2]] %>% View








library(httr)
library(jsonlite)


req_headers <- add_headers(
  `User-Agent`        = "RStudio Scraper email@email.com",
  Accept              = "application/json, text/plain, */*",
  `Accept-Language`   = "en-US,en;q=0.9",
  # httr handles gzip/deflate transparently; listing br/zstd is fine but
  # libcurl will only actually decode what it was built with. Safe to keep:
  `Accept-Encoding`   = "gzip, deflate, br, zstd",
  Referer             = "https://sansad.in/ls/members",
  Connection          = "keep-alive",
  `Sec-Fetch-Dest`    = "empty",
  `Sec-Fetch-Mode`    = "cors",
  `Sec-Fetch-Site`    = "same-origin",
  Priority            = "u=0"
  )

# Query params as a named list — httr builds the query string for you,
# so blanks stay blank just like in the original URL.
qs <- list(
  loksabha             = 18,
  state                = "",
  party                = "",
  gender               = "",
  ageFrom              = "",
  ageTo                = "",
  noOfTerms            = "",
  page                 = 2,
  size                 = 10,
  searchText           = "",
  constituency         = "",
  sitting              = 1,
  locale               = "en",
  month                = "",
  profession           = "",
  otherProfession      = "",
  constituencyCategory = "",
  positionCode         = "",
  qualification        = "",
  noOfChildren         = "",
  isFreedomFighter     = "",
  memberStatus         = "s"
)

resp <- GET(
  "https://sansad.in/api_ls/member",
  query  = qs,
  req_headers,
  timeout(30)
)

stop_for_status(resp)

# Parse JSON. simplifyDataFrame turns membersDtoList into a tibble-friendly DF.
payload <- content(resp, as = "parsed", type = "application/json",
                   simplifyDataFrame = TRUE)

payload$metaDatasDto      # list: currentPageNumber, perPageSize, totalElements, totalPages
members <- payload$membersDtoList
head(members[, c("mpsno", "mpFirstLastName", "partySname", "stateName", "constName")])








