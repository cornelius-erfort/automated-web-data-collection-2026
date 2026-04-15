library(httr)
library(rvest)
library(urltools)
library(stringr)
library(dplyr)
library(jsonlite)
library(lubridate)
library(fixest)

source("functions/unscrambler.R")

if(!dir.exists("data")) dir.create("data")

setwd("data")

##############################
# LOAD LIST OF LOBBY ORGS
##############################

lobbylist <- "Lobbyregistersuche-2024-07-26_15-45-21.json" %>% # "Lobbyregistersuche-Details-2024-01-02_09-59-40.json" %>% # "Lobbyregistersuche-Details-2023-04-20_11-17-27.json" %>% 
  fromJSON

# Versions?
lobbylist$results$registerEntryDetail[[2]]

lobbylist$results %>% View

##############################
# DOWNLOAD THE MAIN PAGE FOR EACH ORG
##############################

# Get all IDs for download
down_ids <- lobbylist$results$registerNumber
down_ids <- down_ids[!(lobbylist$results$registerNumber %in% str_remove(list.files("data"), "\\.html$"))]
# down_ids <- down_ids[down_ids != "R002646"] # 404 error

length(down_ids) # 5,846 # 6,107 # 6,846

# Make dir for pages
if(!dir.exists("pages")) dir.create("pages")

# Remove the ones that are already downloaded
down_ids <- down_ids[!(down_ids %in% str_remove(list.files("pages"), "\\.html$"))]
length(down_ids) # 5,846 # 487

# Download all htmls (redo this to get new versions)
for (id in down_ids) {
  download.file(str_c("https://www.lobbyregister.bundestag.de/suche/", id), str_c("pages/", id, ".html"), quiet = T) %>% try
  
  cat(round(sum((lobbylist$results$registerNumber %in% str_remove(list.files("pages"), "\\.html$")))/length(lobbylist$results$registerNumber)*100, 2), "%\n")
  
}

##############################
# DOWNLOAD ALL VERSIONS FOR EACH ORG
##############################

# Make dir for versions
if(!dir.exists("versions")) dir.create("versions")

# Get all IDs to go through for versioning
versions_ids <- str_remove(list.files("pages"), "\\.html$")
length(versions_ids) # 5,845 # 6,332
# versions_ids <- versions_ids[!(versions_ids %in% dir("versions"))]
# length(versions_ids) # 0

# versions_ids <- versions_ids[versions_ids != "R000383"] # 404 error
# versions_ids <- versions_ids[versions_ids != "R000272"] # 404 error
# versions_ids <- versions_ids[versions_ids != "R000384"] # 404 error
# versions_ids <- versions_ids[versions_ids != "R000446"] # 404 error

for (id in versions_ids) {
  print(id)
  # (length(dir("versions"))/length(list.files("pages"))*100) %>% round(2) %>% cat
  # cat("%\n")
  
  if(!dir.exists(str_c("versions/", id))) dir.create(str_c("versions/", id))
  
  versions <- read_html(str_c("pages/", id, ".html")) %>% 
    html_nodes("option") %>% 
    html_attr("value") %>% 
    str_c("www.lobbyregister.bundestag.de", .)
  
  if (length(versions) > 0) for (version in versions) if(!file.exists(str_c("versions/", id, "/", basename(version), ".html"))) download.file(version, str_c("versions/", id, "/", basename(version), ".html"), quiet = T)  %>% try
  
}











