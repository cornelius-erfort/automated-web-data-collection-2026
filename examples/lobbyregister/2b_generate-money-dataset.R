library(httr)
library(rvest)
library(urltools)
library(stringr)
library(dplyr)
library(jsonlite)
library(lubridate)
library(fixest)

source("scripts/unscrambler.R")

setwd("data")



###############################
# EXTRACT MONEY VARS FROM VERSIONS
############################### 

# Do the versions also include the most recent version (i.e. main page)?

money_panel <- data.frame()

for (dir in (list.dirs("versions") %>% str_subset("/"))) for (file in list.files(dir, full.names = T)) {
  print(file)
  
  if (file %in% money_panel$file) next
  if (file.size(file) == 0) {
    file.remove(file) 
    next
  }
  
  myhtml <- read_html(file, encoding = "UTF-8")
  mynodes <- myhtml %>% html_nodes(".info-wrapper")
  (update_date <- mynodes[mynodes %>% as.character %>% str_detect("Letzte Ä")] %>% html_nodes(".element-lg-block") %>% html_text() %>% dmy)
  (first_date <- mynodes[mynodes %>% as.character %>% str_detect("Erst")] %>% html_nodes(".element-lg-block") %>% html_text() %>% dmy)
  
  mynodes <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-donators ul")
  mynodes <- mynodes[1]
  
  public <- data.frame(type = "public_allowance")
  
  length(mynodes)
  
  if (length(mynodes) > 0) {
    (myyear <- (myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-donators ul") %>% html_nodes(".info-title div"))[1] %>% html_text())
    public <- data.frame(year = ifelse(length(myyear) > 0, myyear, NA)) %>% cbind(public)
    
    if(length(mynodes %>% html_nodes("ol li")) > 0) {
      public <- cbind(public, origin = mynodes %>% html_nodes("ol li") %>% html_node("strong") %>% html_text %>% str_trim("both"))
      public <- cbind(public, amount = mynodes %>% html_nodes("ol li") %>% html_node(".amount") %>% html_text %>% str_trim("both"))
      public <- cbind(public, location = mynodes %>% html_nodes("ol li") %>% html_node(".location") %>% html_text %>% str_trim("both"))
      public <- cbind(public, description = mynodes %>% html_nodes("ol li") %>% html_node(".description") %>% html_text %>% str_trim("both"))
      
    }
    
  } else {
    mynodes <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-public-allowances") %>% html_nodes("ul li ol") 
    myyear <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-public-allowances p:nth-child(2)") %>% html_text2
    
    public <- data.frame(year = ifelse(length(myyear) > 0, myyear, NA)) %>% cbind(public)
    
    if(length(mynodes) > 0) {
      
      (origin <- ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), (mynodes %>% html_nodes("li") %>% html_node("strong:first-child") %>% html_text %>% str_trim("both")), NA))
      public <- cbind(public, origin = origin)
      
      
      (amount <-  ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node(".amount") %>% html_text %>% str_trim("both"), NA))
      if(all(is.na(amount))) (amount <-  ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node("span") %>% html_text %>% str_trim("both"), NA))
      
      public <- cbind(public, amount = amount)
      
      (description <-  ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node(".description") %>% html_text %>% str_trim("both"), NA))
      if(all(is.na(description))) description <- ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node("div div:last-child") %>% html_text %>% str_trim("both"), NA)
      
      public <- cbind(public, description = description)
      
      (location <-  ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node(".location") %>% html_text %>% str_trim("both"), NA))
      if(all(is.na(location))) location <- ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node("div div:nth-child(4)") %>% html_text %>% str_trim("both"), NA)
    
      public <- cbind(public, location = location)
      

      # mynodes %>% html_nodes("li") %>% html_node("div div:nth-child(3)") %>% html_text
      
      
      }
  }
  
  mynodes <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-donators ul") 
  mynodes <- mynodes[2]
  
  private <- data.frame(type = "donation")
  
  length(mynodes)
  
  if (length(mynodes) > 0) {
    (myyear <- (myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-donators ul:last-child") %>% html_nodes(".info-title div"))[1] %>% html_text())
    (private <- data.frame(year = ifelse(length(myyear) > 0, myyear, NA)) %>% cbind(private))
    
    if(length(mynodes %>% html_nodes("ol li")) > 0) {
      origin <- mynodes %>% html_nodes("ol li") 
      if(str_detect(origin %>% html_nodes("strong.name span") %>% html_attrs, "encry") %>% suppressWarnings() %>% any) origin <- origin %>% html_node("strong")  %>% html_text %>% str_trim("both") %>% sapply(unscrambler) else{
        origin <- origin %>% html_node("strong")  %>% html_text %>% str_trim("both")
      }
      
      private <- cbind(private, origin = origin)
      private <- cbind(private, amount = mynodes %>% html_nodes("ol li") %>% html_node(".amount") %>% html_text %>% str_trim("both"))
      # private <- cbind(private, location = mynodes %>% html_nodes("ol li") %>% html_node(".location") %>% html_text %>% str_trim("both"))
      private <- cbind(private, description = mynodes %>% html_nodes("ol li") %>% html_node(".description") %>% html_text %>% str_trim("both"))
      
    }
    
  }  else {
    mynodes <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-donations") %>% html_nodes("ul li ol") 
    (myyear <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-donations p:nth-child(2)") %>% html_text2)
    
    
    private <- data.frame(year = ifelse(length(myyear) > 0, myyear, NA)) %>% cbind(private)
    
    if(length(mynodes) > 0) {
      
      
      (origin <- ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), (mynodes %>% html_nodes("li") %>% html_node("strong:first-child") %>% html_text %>% str_trim("both")), NA))
      private <- cbind(private, origin = origin)
      
      (amount <-  ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node(".amount") %>% html_text %>% str_trim("both"), NA))
      if(all(is.na(amount))) (amount <-  ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node("span") %>% html_text %>% str_trim("both"), NA))
      
      private <- cbind(private, amount = amount)
      
      (description <-  ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node(".description") %>% html_text %>% str_trim("both"), NA))
      if(all(is.na(description))) description <- ifelse(rep(length(mynodes) > 0, length(mynodes %>% html_nodes("li"))), mynodes %>% html_nodes("li") %>% html_node("div div:last-child") %>% html_text %>% str_trim("both"), NA)
      
      private <- cbind(private, description = description)
      
      
    }
  }
  
  org_name <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title h2") %>% html_text2
  if(myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title span") %>% html_attrs %>% str_detect("encrypted") %>% any %>% suppressWarnings()) org_name <- org_name %>% unscrambler %>% str_trim("both")
  
  thisentry <- data.frame(org_name = org_name, register_number = dir %>% str_extract("(?<=/).*"), version_id = file %>% str_extract("(?<=/)[:digit:]*(?=\\.html$)"),
                          update_date = update_date, first_date = first_date, file = file) %>% cbind(public)
  
  thisentry <- data.frame(org_name = org_name, register_number = dir %>% str_extract("(?<=/).*"), version_id = file %>% str_extract("(?<=/)[:digit:]*(?=\\.html$)"),
                          update_date = update_date, first_date = first_date, file = file) %>% cbind(private) %>% bind_rows(thisentry)
  
  money_panel <- bind_rows(money_panel, thisentry)
  
}

save(money_panel, file = "money_panel.rda")
# Needs to be checked

##############################
# CREATE MORE VARS
##############################

money_panel$year <- money_panel$year %>% str_remove("Geschäftsjahr: ")

money_panel$fiscal_year <- NA
money_panel$fiscal_year[str_detect(money_panel$year, "^01/20")] <- 2020
money_panel$fiscal_year[str_detect(money_panel$year, "^01/21")] <- 2021
money_panel$fiscal_year[str_detect(money_panel$year, "^01/22")] <- 2022
money_panel$fiscal_year[str_detect(money_panel$year, "^01/23")] <- 2023

table(money_panel$fiscal_year)
money_panel$grant_amount_est <- money_panel$amount %>% str_extract_all("(\\d{1,3}\\.)?(\\d{1,3}\\.)?(\\d{1,3}\\.)?\\d{1,3}", simplify = T)


money_panel$grant_amount_est <- (money_panel$grant_amount_est[, 1] %>% str_remove_all("\\.") %>%  as.numeric() - 1 + money_panel$grant_amount_est[, 2] %>% str_remove_all("\\.") %>%  as.numeric())/2
money_panel$grant_amount_est[is.na(money_panel$grant_amount_est)] <- 0

# money_panel <- filter(money_panel, !is.na(fiscal_year) & !is.na(grant_amount_est) & type == "public_allowance")

# money_panel$description <- money_panel$description %>%  str_replace_all(c("Ã¼" = "ü", "Ã¶" = "ö", "â¬" = "€", "Ã¤" = "ä", "Ã\u009f" = "ß", "Ã\u0084" = "Ä", "Ã\u0096" = "Ö", "Ã\u009c" = "Ü", "Â§" = "§"))
# money_panel$origin <- money_panel$origin %>%  str_replace_all(c("Ã¼" = "ü", "Ã¶" = "ö", "â¬" = "€", "Ã¤" = "ä", "Ã\u009f" = "ß", "Ã\u0084" = "Ä", "Ã\u0096" = "Ö", "Ã\u009c" = "Ü", "Â§" = "§"))

save(money_panel, file = "money_panel.rda")


##############################
# AGGREGATE THE DATA TO ONE ROW PER ORG AND YEAR
##############################

# Remove duplicates (this can be improved by including duplicates within the same version)
money_panel_agg <- aggregate(grant_amount_est ~ register_number + fiscal_year, unique(select(money_panel, c(register_number, fiscal_year, description, grant_amount_est))), sum)
save(money_panel_agg, file = "money_panel_agg.rda")














##############################
# ARCHIVE
##############################



lobbyists <- myhtml %>% html_nodes(".employees-entrusted-persons .mod-common-search-person-name") %>% html_text() %>% str_trim("both") # New htmls


if(length(lobbyists) == 0) lobbyists <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-identity-finances ol li span.mod-common-search-person-name") %>% html_text() %>% str_trim("both") # Old htmls

if(length(ver_persons) == 0 & length(lobbyists) == 0) people <- data.frame()

if(length(ver_persons) > 0) people <- data.frame(name = ver_persons[which(str_detect(ver_persons, "Funktion: ")) - 1],
                                                 role = ver_persons[which(str_detect(ver_persons, "Funktion: "))] %>% str_remove("Funktion: "),
                                                 number = 1:length(ver_persons[which(str_detect(ver_persons, "Funktion: ")) - 1]))

if(length(lobbyists) > 0)  people <- data.frame(name = lobbyists,
                                                role = "lobbyist",
                                                number = 1:length(lobbyists)) %>% bind_rows(people, .)

if(myhtml %>% html_nodes(".mod-common-search-person-name span") %>% html_attrs %>% str_detect("encrypted") %>% any %>% suppressWarnings()) people$name <- people$name %>% sapply(unscrambler)






(funding_sources <- myhtml %>% html_nodes(".main-funding-sources") %>% html_text2)
if(length(funding_sources) == 0) funding_sources <- NA
# lobby_money <- (myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-identity-finances ul li:nth-child(1)") %>% html_text)[1] %>% str_remove("J.*hrliche finanzielle Aufwendungen im Bereich der Interessenvertretung:") %>% str_trim("both") 
# lobby_people <- (myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-identity-finances ul li:nth-child(2)") %>% html_text)[1] %>% str_remove("Anzahl der Besch.*ftigten im Bereich der Interessenvertretung:") %>% str_trim("both")


# "versions/R000015/32181.html"
org_name <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title h2") %>% html_text2
if(myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title span") %>% html_attrs %>% str_detect("encrypted") %>% any %>% suppressWarnings()) org_name <- org_name %>% unscrambler %>% str_trim("both")


mynodes <- myhtml %>% html_nodes(".info-wrapper")
update_date <- mynodes[mynodes %>% as.character %>% str_detect("Letzte Änderung")] %>% html_nodes(".element-lg-block") %>% html_text() %>% dmy
first_date <- mynodes[mynodes %>% as.character %>% str_detect("Erst")] %>% html_nodes(".element-lg-block") %>% html_text() %>% dmy

thisentry <- data.frame(org_name = org_name,
                        register_number = dir %>% str_extract("(?<=/).*"), 
                        version_id = file %>% str_extract("(?<=/)[:digit:]*(?=\\.html$)"),
                        update_date = update_date, 
                        first_date = first_date, 
                        file = file, 
                        lobby_money = lobby_money, 
                        lobby_people = lobby_people,
                        funding_sources = funding_sources) 

if (nrow(people) > 0) thisentry <- cbind(thisentry, people)


##############################
# MAKE MORE VARS
##############################


# people_panel$lobbyist_name <- people_panel$lobbyist_name %>%  str_replace_all(c("Ä±" = "ı", "  " = " ", "Å\u009f" = "ş", "Ã¼" = "ü", "Ã¶" = "ö", "â¬" = "€", "Ã¤" = "ä", "Ã\u009f" = "ß", "Ã\u0084" = "Ä", "Ã\u0096" = "Ö", "Ã\u009c" = "Ü", "Â§" = "§", "Ã©" = "é", "Ã¡" = "á", "Ã±"="ñ", "Ã³" = "ó", "Ã" = "í", "í\u0089" = "É"))

# people_panel$lobby_money <- people_panel$lobby_money %>%  str_replace_all(c("Ä±" = "ı", "  " = " ", "Å\u009f" = "ş", "Ã¼" = "ü", "Ã¶" = "ö", "â¬" = "€", "Ã¤" = "ä", "Ã\u009f" = "ß", "Ã\u0084" = "Ä", "Ã\u0096" = "Ö", "Ã\u009c" = "Ü", "Â§" = "§", "Ã©" = "é", "Ã¡" = "á", "Ã±"="ñ", "Ã³" = "ó", "Ã" = "í", "í\u0089" = "É"))

# people_panel$lobbyist_name[str_detect(people_panel$lobbyist_name , "Pénz") & !is.na(people_panel$lobbyist_name)]

# Add fiscal year
people_panel$year <- people_panel$lobby_money %>% str_extract("Geschäftsjahr:.*") %>% str_remove("Geschäftsjahr: ")
table(people_panel$year)
people_panel$fiscal_year <- NA
people_panel$fiscal_year[str_detect(people_panel$year, "^01/20")] <- 2020
people_panel$fiscal_year[str_detect(people_panel$year, "^01/21")] <- 2021
people_panel$fiscal_year[str_detect(people_panel$year, "^01/22")] <- 2022
people_panel$fiscal_year[str_detect(people_panel$year, "^01/23")] <- 2023

table(people_panel$fiscal_year)

people_panel$lobbying_amount_est <- people_panel$lobby_money %>% str_replace_all(c("\r" = " ", "\t" = " ", "\n" = " ")) %>%  str_extract("(?<=bis \\d\\d\\/\\d\\d).*") %>%  str_extract_all("(\\d{1,3}\\.)?(\\d{1,3}\\.)?(\\d{1,3}\\.)?\\d{1,3}", simplify = T)

# Take the the center of the interval as the value
people_panel$lobbying_amount_est <- (people_panel$lobbying_amount_est[, 1] %>% str_remove_all("\\.") %>%  as.numeric() - 1 + people_panel$lobbying_amount_est[, 2] %>% str_remove_all("\\.") %>%  as.numeric())/2

# Fix zeros
people_panel$lobbying_amount_est[str_detect(people_panel$lobby_money %>% str_replace_all(c("\r" = " ", "\t" = " ", "\n" = " ")), "/\\d\\d *0 Euro")] <- 0

# Do the same for the number of people (in intervals)
people_panel$people_est <- people_panel$lobby_people %>% str_extract("Anzahl der Beschäftigten im Bereich der Interessenvertretung:(\n|\r|\t<|.)*") %>%  str_extract_all("\\d{1,4}", simplify = T)
people_panel$people_est <- (people_panel$people_est[, 1] %>% str_remove_all("\\.") %>%  as.numeric() - 1 + people_panel$people_est[, 2] %>% str_remove_all("\\.") %>%  as.numeric())/2
people_panel$people_est[people_panel$lobby_people == "Anzahl der Beschäftigten im Bereich der Interessenvertretung:\n0"] <- 0

people_panel$lobby_pos <- people_panel$lobby_people %>% str_extract("Vollzeitäquivalent der im Bereich der Interessenvertretung beschäftigten Personen:(\n|\r|\t<|.)*") %>%  str_extract_all("\\d{1,4},\\d{1,4}", simplify = T) %>% str_replace(",", ".") %>%  as.numeric()


# Count number of named lobbyists
people_panel <- people_panel %>% group_by(register_number, version_id) %>% mutate(named_lobbyists = n()) 
                                                                                      
people_panel <- people_panel %>% dplyr::group_by(register_number, version_id) %>% dplyr::mutate(named_lobbyists = ifelse(test = all(is.na(lobbyist_name)), yes = 0, no = named_lobbyists))

people_panel <- unique(people_panel)


lobbyist_names$duplicated <- lobbyist_names %>% select(register_number, name) %>% duplicated(fromLast = T)


people_panel$update_date[is.na(people_panel$update_date)] <- people_panel$first_date[is.na(people_panel$update_date)]



save(people_panel, file = "people_panel.rda")




##############################
# AGGREGATE THE DATA TO ONE ROW PER ORG AND YEAR
##############################



# Here we need to distinguish between the estimated expenditures for the fiscal_year, and the people info for the update_date
# people_panel_agg <- aggregate(cbind(lobbying_amount_est, people_est, named_lobbyists) ~ register_number + fiscal_year, data = people_panel_agg, FUN = max) # Old version

people_panel_agg_money <- unique(select(people_panel, c(register_number, fiscal_year, lobbying_amount_est)))
people_panel_agg_money <- aggregate(cbind(lobbying_amount_est) ~ register_number + fiscal_year, data = people_panel_agg_money, FUN = max)


people_panel_agg_people <- unique(select(people_panel, c(register_number, update_date, people_est, named_lobbyists)))
people_panel_agg_people$fiscal_year <- people_panel_agg_people$update_date %>% str_sub(1, 4) %>% as.numeric()
people_panel_agg_people <- aggregate(cbind(people_est, named_lobbyists) ~ register_number + fiscal_year, data = people_panel_agg_people, FUN = max)


people_panel_agg <- merge(people_panel_agg_money, people_panel_agg_people, by = c("fiscal_year", "register_number"), all = T)

save(people_panel_agg, file = "people_panel_agg.rda")

# origin is sometimes scrambled! Use unscrambler if necessary (detect scrambled data by too many consonants?)
money_lobby <- merge(people_panel_agg, money_panel_agg, by = c("fiscal_year", "register_number"), all = T)

##############################
# ADD LOBBY ORG TYPE FROM LOBBYREGISTER DATA
##############################

# Get list of lobby orgs
lobbylist <- "Lobbyregistersuche-Details-2024-01-02_09-59-40.json" %>% # "Lobbyregistersuche-Details-2023-04-20_11-17-27.json" %>% 
  fromJSON

# Extract type
acitvities <- data.frame(activitiy_de = lobbylist$results$registerEntryDetail$activity$de, activity_en = lobbylist$results$registerEntryDetail$activity$en, register_number = lobbylist$results$registerNumber)

dim(money_lobby)

# Merge
money_lobby <- merge(money_lobby, acitvities, all.x = T, by = "register_number")

dim(money_lobby)

# Add abbreviation of type
money_lobby$activity_combined <- money_lobby$activity_en %>% recode( "Science, think tank, research institution or college" = "Science", 
                                                                     "Organization under private law with public welfare tasks (e.g. registered associations, foundations)" = "Publ. welfare",
                                                                     "Non-governmental organizations (non-governmental organization, platform or network)" = "NGO",
                                                                     "Church or other religious or ideological community" = "Other",
                                                                     "Consulting company, independent consultant" = "Consultancy",
                                                                     "Public law organization (e.g. corporations, institutions and foundations under public law)" = "Other",
                                                                     "Law firm, individual attorney" = "Other",
                                                                     "Workers association" = "Other",
                                                                     "Professional association" = "Prof. assn.",
                                                                     "Employers association" = "Employers",
                                                                     "Trade association" = "Trade assn.")

save(money_lobby, file = "money_lobby.rda")
