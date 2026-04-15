# Scraping the polls from Politico Europe
# Author: Cornelius Erfort
# Date: Sys.Date()
#
# This script downloads and processes polling data from https://www.politico.eu/europe-poll-of-polls
#
# Requirements: scripts/packages.R, data/parties-import.xlsx

# ---- Load Packages and Functions ----
source("scripts/packages.R")

# ---- Parameters ----
output_dir <- "polls-json"
data_dir <- "data"
parties_file <- file.path(data_dir, "parties-import.xlsx")
polls_output_file <- file.path(data_dir, "polls-all.RDS")

# ---- Author and HTTP Header ----

# ---- Helper Functions ----
download_country_json <- function(country, output_dir) {
  file <- file.path(output_dir, paste0(country, ".RDS"))
  if (file.exists(file)) return(invisible(NULL))
  url <- paste0("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/",
                gsub("UK", "GB", country), "-parliament")
  message("Downloading: ", url)
  tryCatch({
    resp <- httr::GET(url)
    content <- httr::content(resp)
    saveRDS(content, file)
  }, error = function(e) {
    warning("Failed to download for ", country, ": ", e$message)
  })
}

process_country_polls <- function(country, output_dir) {
  file <- file.path(output_dir, paste0(country, ".RDS"))
  if (!file.exists(file)) return(NULL)
  json <- readRDS(file)
  parties <- data.frame(party = names(json$parties), party_name = as.character(json$parties))
  polls <- plyr::rbind.fill(lapply(json$polls, data.frame, stringsAsFactors = FALSE))
  if (nrow(polls) == 0) return(NULL)
  polls$country <- country
  polls <- tidyr::pivot_longer(polls, cols = dplyr::starts_with("parties."),
                              names_pattern = "parties.(.*)", names_to = "party", values_to = "percent")
  polls <- merge(polls, parties, all.x = TRUE, by = "party")
  return(polls)
}

# ---- Main Execution ----

# 1. Get main polls page and country list
main_page <- httr::GET("https://www.politico.eu/europe-poll-of-polls") %>% rvest::content()
countries <- data.frame(
  country = rvest::html_nodes(main_page, "a.ee-flags__anchor") %>% rvest::html_text(),
  url = rvest::html_nodes(main_page, "a.ee-flags__anchor") %>% rvest::html_attr("href"),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(country_name = basename(url)) %>%
  dplyr::filter(country != "home") %>%
  unique()
countries$country <- stringr::str_trim(countries$country, "both")
countries <- dplyr::filter(countries, !(country %in% c("home", "EU")))

# 2. Download JSON files for each country
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
for (country in countries$country) {
  download_country_json(country, output_dir)
}

# 3. Construct dataframe for all polls
all_polls <- plyr::rbind.fill(lapply(countries$country, process_country_polls, output_dir = output_dir))

if (is.null(all_polls) || nrow(all_polls) == 0) {
  stop("No polling data was downloaded or processed.")
}

# 4. Add parlgov_id from external file
if (file.exists(parties_file)) {
  parties <- openxlsx::read.xlsx(parties_file) %>%
    dplyr::select(country, party, parlgov_id) %>%
    unique() %>%
    dplyr::filter(!is.na(party))
  all_polls <- dplyr::filter(all_polls, !is.na(percent))
  all_polls <- merge(all_polls, parties, all.x = TRUE, by = c("country", "party"))
  message("Number of polls without parlgov_id: ", sum(is.na(all_polls$parlgov_id)))
} else {
  warning("Parties file not found: ", parties_file)
}

# 5. Save all polls
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(all_polls, polls_output_file)
message("Saved all polls to ", polls_output_file)

# 6. Summary output
print(dim(all_polls))
print(dplyr::group_by(all_polls, country) %>%
        dplyr::summarise(min_date = min(date),
                         max_date = max(date),
                         n_obs = dplyr::n()))
print(table(all_polls$country)) 