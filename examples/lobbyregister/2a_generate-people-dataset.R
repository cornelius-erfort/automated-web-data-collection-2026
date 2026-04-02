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


##############################
# EXTRACT PEOPLE VARS FROM VERSIONS
##############################




# Get exact times for updates
# exact_times <- data.frame()
# for (file in list.files("pages", full.names = T)) {
#   print(file)
#   myhtml <- read_html(file, encoding = "UTF-8")
#   exact <- data.frame(file = myhtml %>% html_nodes("select option") %>% html_attr("value") %>% str_replace("/suche", "versions") %>% str_c(".html"),
#              update_info = myhtml %>% html_nodes("select option") %>% html_attr("data-text-long"))
#   
#   exact$yearly_update <- str_detect(exact$info, "jährliche")
#   (exact$from <- str_extract(exact$info, "(?<=(vom|seit) )\\d{2}\\.\\d{2}\\.\\d{4}  \\d{2}\\:\\d{2}\\:\\d{2}") %>% as.POSIXct(tz = Sys.timezone(), format="%d.%m.%Y  %H:%M:%S"))
#   (exact$until <- str_extract(exact$info, "(?<=(bis) )\\d{2}\\.\\d{2}\\.\\d{4}  \\d{2}\\:\\d{2}\\:\\d{2}") %>% as.POSIXct(tz = Sys.timezone(), format="%d.%m.%Y  %H:%M:%S"))
#   exact_times <- bind_rows(exact_times, exact)
# }
# save(exact_times, file = "exact_times.rda")
# load("exact_times.rda")

lobby_panel <- data.frame()
people_panel <- data.frame()
auftrage_people_panel <- data.frame()
auftraggeber_people_panel <- data.frame()
auftrage_panel <- data.frame()
auftraggeber_panel <- data.frame()

mydirs <- (list.dirs("versions") %>% str_subset("/"))
mydirs <- mydirs[sample(length(mydirs), length(mydirs))]

# length(mydirs)
# lobby_panel$file %>% dirname() %>% unique %>% length


for (dir in mydirs) {
  (file.list <- list.files(dir, full.names = T))
  for (file in file.list) {
  
  if (file %in% lobby_panel$file) next
  if (file.size(file) == 0) {
    file.remove(file) 
    next
  }
  print(file)
    
  myhtml <- read_html(file, encoding = "UTF-8")

  mynodes <- myhtml %>% html_nodes(".info-wrapper")
  (update_date <- mynodes[mynodes %>% as.character %>% str_detect("Letzte Änderung")] %>% html_nodes(".element-lg-block") %>% html_text() %>% dmy)
  (first_date <- mynodes[mynodes %>% as.character %>% str_detect("Erst")] %>% html_nodes(".element-lg-block") %>% html_text() %>% dmy)
  
  
  (lobby_people <- myhtml %>% html_nodes(".number-of-employees") %>% html_text2)
  if(length(lobby_people) == 0)   (lobby_people <- (myhtml %>% html_nodes("div.mod-common-search-register-entry-content-information-identity-finances ul li.col-sm-6:nth-child(2)") %>% html_text2)[1])
  if(length(lobby_people) == 0) lobby_people <- NA
  
  (lobby_money <- myhtml %>% html_nodes(".financial-expenses") %>% html_text2)
  if(length(lobby_money) == 0)   (lobby_money <- (myhtml %>% html_nodes("div.mod-common-search-register-entry-content-information-identity-finances ul:first-child li:first-child") %>% html_text2)[1])
  if(length(lobby_money) == 0) lobby_money <- NA
  
  (funding_sources <- myhtml %>% html_nodes(".main-funding-sources") %>% html_text2)
  if(length(funding_sources) == 0) funding_sources <- NA
  
  (update_info <- myhtml %>% html_nodes("select option"))
  (update_info <- update_info[!is.na(update_info %>% html_attr("selected"))] %>% html_attr("data-text-long"))
  if(length(update_info) == 0) update_info <- NA
  
    # MAIN PROFILE (not Auftraggeber
    main_profile <- data.frame(file = file,
                               update_date = update_date,
                               first_date = first_date,
                               update_info = update_info,
                               org_name = myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title h2") %>% html_text2,
                               lobby_people = lobby_people,
                               lobby_money = lobby_money
                               )
    # Act type
    (act_type <- myhtml %>% html_nodes(".info-wrapper div") %>% html_text2)
    if(length(act_type) > 0) main_profile$act_type <- act_type
    
    # Org type
    (org_type <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title p") %>% html_text2)
    if(length(org_type) > 0) main_profile$org_type <- org_type
    
    # Org name
    (org_name <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title h2") %>% html_text2 %>% str_remove_all("\r ?"))
    # Decrypt if necessary
    if(myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title h2 span") %>% html_attrs %>% str_detect("encr") %>% any %>% suppressWarnings()) (main_profile$org_name <- unscrambler(org_name) %>% unname)
    
    (inactive_date <- myhtml %>% html_nodes(".mod-common-inactive-status") %>% html_text2) #  span:last-child
    if(length(inactive_date) > 0) main_profile$inactive_date <- inactive_date
    
    (gov_pos <- myhtml %>% html_nodes(".mod-common-search-register-entry-content div > div.mod-common-revolving-door-data") %>% html_text2)
    if(length(gov_pos) != 0) main_profile$gov_pos <- gov_pos

    lobby_panel <- bind_rows(lobby_panel, main_profile)
    
    # Vertretungsberechtigte Personen
    (ver_persons <- myhtml %>% html_nodes(".authorized-members .info-element"))
    
    # Names
    (ver_persons_name <- ver_persons %>% html_nodes(".keep-spaces") %>% html_text2 %>% str_trim("both")  %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist %>% lapply(unname))
    # Decrypt if necessary
    if(ver_persons %>% html_nodes(".keep-spaces") %>% html_attrs %>% str_detect("encry") %>% any %>% suppressWarnings) (ver_persons_name <- lapply(ver_persons_name, function(x) sapply(x, unscrambler)) %>% lapply(unname))
    
    ver_persons_df <- data.frame()
    if(length(ver_persons) != 0) {
      ver_persons_df <- data.frame(name = ver_persons_name %>% unlist,
                                   fun = ver_persons %>% lapply(function (x) html_nodes(x, "ul:first-child > li:nth-child(2)") %>% html_text2) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist %>% str_trim("both"),
                                   # ver_persons %>% lapply(function (x) html_nodes(x, "div:not(.mod-common-revolving-door-data)")  %>% html_text2) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist,
                                   gov_pos = ver_persons %>% lapply(function (x) html_nodes(x, "div.mod-common-revolving-door-data") %>% html_text2 %>% str_trim("both")) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist)
    } else { # Old htmls
      # Vertretungsberechtigte Personen
      (ver_persons <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-identity-finances div.mod-common-search-register-entry-legal-representative > ul.row"))
      
      (ver_persons_name <- ver_persons %>% html_nodes("li > ul > li:first-child > span.mod-common-search-person-name") %>% html_text2 %>% str_trim("both"))
      if(ver_persons %>% html_nodes("li > ul > li:first-child > span.mod-common-search-person-name span") %>% html_attrs %>% str_detect("encry") %>% any %>% suppressWarnings) (ver_persons_name <- ver_persons_name %>% sapply(unscrambler) %>% unname)
      
      (ver_persons_fun <- ver_persons %>% html_nodes("li > ul > li > div")  %>% html_text2 %>% str_subset("Funktion: ") %>% str_trim("both"))
      
      if(length(ver_persons) != 0) {
        
        ver_persons_df <- data.frame(name = ver_persons_name,
                                   fun = ver_persons_fun
        )
      
        
      }
    }
    if(nrow(ver_persons_df) != 0) ver_persons_df$person_id <- 1:nrow(ver_persons_df)
    

    # Lobbyists
    (lobbyists <- myhtml %>% html_nodes(".employees-entrusted-persons li"))
    
    # Decrypt lobbyists name?
    lobbyists_df <- data.frame()
    if(length(lobbyists) != 0) {
      lobbyists_df <- data.frame(name = lobbyists %>% lapply(function (x) html_nodes(x, ".keep-spaces") %>% html_text2 %>% str_trim("both"))  %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist,
                                 fun = "lobbyist",
                                 gov_pos = lobbyists %>% lapply(function (x) html_nodes(x, "div") %>% html_text2 %>% str_trim("both"))  %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist
                                 )
    } else { # Old htmls
      # Lobbyists
      (lobbyists <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-identity-finances li.col-sm-12.info-element:not(.memberships) > ol > li"))
      
      # lobbyists %>% html_text2 %>% str_trim("both") %>% sapply(unscrambler) %>% unname
      
      if(length(lobbyists) != 0) {
        
        lobbyists_df <- data.frame(name = lobbyists %>% html_text2 %>% str_trim("both"),
                                   fun = "lobbyist"
        )
        
        if(lobbyists %>% html_nodes("span") %>% html_attrs %>% str_detect("encry") %>% any %>% suppressWarnings) lobbyists_df$name <- lobbyists_df$name %>% sapply(unscrambler) %>% unname
      
      }
    }
    if(nrow(lobbyists_df) != 0) lobbyists_df$person_id <- 1:nrow(lobbyists_df)
    

    persons_df <- bind_rows(ver_persons_df, lobbyists_df)
    if(nrow(persons_df) > 0) {
      persons_df <- cbind(select(main_profile, c(file)), persons_df)
      people_panel <- bind_rows(people_panel, persons_df)
    }
    
    ########
    # AUFTRÄGE AND AUFTRAGGEBER
    
    if(length(myhtml %>% html_nodes(".contract-clients-list")) != 0) {  
      
    # AUFTRÄGE
    (auftrage <- myhtml %>% html_nodes(".contracts-list > li"))

    if(length(auftrage) > 0) for (i in 1:length(auftrage)) {
      
      # i = 12
      auftrag <- auftrage[[i]]
      
      this_auftrag <- data.frame(file = file,
                                    auftrag_id = i,
                                    page_type = "new",
                                    sub_string = auftrag %>% list %>%  lapply(function (x) html_nodes(x, "ol.contract-clients-list") %>% html_text2) %>% unlist
                                   
      ) 
      auftrage_panel <- bind_rows(auftrage_panel, this_auftrag)  
      
      # Betraute personen
      betraute_personen <- data.frame(
      sub_name = auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-entrusted-persons-organization .keep-spaces") %>% html_text2) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist,
      sub_gov_pos = auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-entrusted-persons-organization  li")) %>% lapply(function (x) sapply(x, function (y) html_nodes(y, ".mod-common-revolving-door-data") %>% html_text2) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist  )  %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist,
      sub_type = "betraute_person"
      ) %>% filter(!is.na(sub_name))
      
      if(nrow(betraute_personen) > 0) {
        betraute_personen$sub_id <- 1:nrow(betraute_personen)
        betraute_personen <- cbind(this_auftrag %>% select(c(file, auftrag_id)), betraute_personen)
        auftrage_people_panel <- bind_rows(auftrage_people_panel, betraute_personen)
      }
      
      if (!(file %in% c("versions/R002108/30592.html", "versions/R003046/32028.html", "versions/R002324/40430.html", "versions/R002324/40484.html"))) { # In this file, the Unterauftragnehmer (which is also the registrant!) has Vetretungsb. Personen and Eingesetzte Personen (a nesting structure unseen before)
        
      
      # Unterauftragnehmer
      unterauftragnehmer <- data.frame(
        sub_name = auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors div.info-title") %>% html_text  %>% str_trim("both")) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist,
        sub_link = auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors a") %>% html_attr("href")) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist,
        sub_type = "subcontractor"
      ) 
      
      (sub_is_person <- (auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors li")))[[1]] %>% lapply(function (x) html_nodes(x, ".mod-common-search-person-name")) %>% lapply(function (x) length(x)) %>% as.logical)
      if(length(sub_is_person) > 0) (unterauftragnehmer$sub_is_person <- sub_is_person)
      unterauftragnehmer <- unterauftragnehmer %>% filter(!is.na(sub_name) | !is.na(sub_link))
      if(nrow(unterauftragnehmer) > 0) {
        unterauftragnehmer$sub_id <- 1:nrow(unterauftragnehmer)
        unterauftragnehmer <- cbind(this_auftrag %>% select(c(file, auftrag_id)), unterauftragnehmer)
        auftrage_people_panel <- bind_rows(auftrage_people_panel, unterauftragnehmer)
      }
      }

      
      # AUFTRAGGEBER 
      # There could be more than one auftraggeber per auftrag, but only one list of entrusted persons
      (auftraggeber <- html_nodes(auftrag, ".contract-clients-list > li.info-element"))
      
      if(length(auftraggeber) > 0) for (j in 1:length(auftraggeber)) {
        
        # j = 1
        (auftraggeber_name <- auftraggeber[[j]] %>% html_nodes("div strong > .keep-spaces") %>% html_text2)
        if(length(auftraggeber_name) == 0) (auftraggeber_name <- auftraggeber[[j]] %>% html_nodes(".keep-spaces") %>% html_text  %>% str_trim("both"))
        
        this_auftraggeber <- data.frame(file = file,
                                        auftrag_id = i,
                                        auftraggeber_id = j,
                                        page_type = "new",
                                        auftraggeber_name = auftraggeber_name)
        
        # Auftrag pay
        (auftrag_pay <- auftraggeber[[j]] %>% html_nodes("li") %>% html_text2 %>% str_subset("Erhaltene "))
        if(length(auftrag_pay) == 0) (auftrag_pay <- auftraggeber[[j]] %>% html_nodes("li.info-element div") %>% html_text2 %>% str_subset("Erhaltene "))
        if(length(auftrag_pay) == 0) (auftrag_pay <- auftraggeber[[j]] %>% html_nodes("div") %>% html_text2 %>% str_subset("Erhaltene "))
        if(length(auftrag_pay) > 0) this_auftraggeber$auftrag_pay <- auftrag_pay
        
        # Org type
        (auftraggeber_org_type <- auftraggeber[[j]] %>% html_nodes("div.info-title > div") %>% html_text2 %>% str_trim("both"))
        if(length(auftraggeber_org_type) > 0) this_auftraggeber$org_type <- auftraggeber_org_type
        
        # Link
        (auftraggeber_link <- auftraggeber[[j]] %>% html_nodes(".info-title .keep-spaces") %>% html_attr("href"))
        if(length(auftraggeber_link) > 0) this_auftraggeber$auftraggeber_link <- auftraggeber_link
        
        # Vertretungsb. Personen der Auftraggeber (could be nested)
        (vb_persons <- html_nodes(auftraggeber[[j]], ".mod-common-search-register-entry-legal-representative ol > li"))
        
        vb_persons_df <- data.frame()
        if(length(vb_persons %>% unlist) > 0) for (k in 1:length(vb_persons)) {

          this_vb <- data.frame( # 
            vb_id = k,
            vb_name = vb_persons[[k]] %>% html_nodes(".mod-common-search-person-name") %>% html_text2,
            vb_fun = vb_persons[[k]]  %>% html_nodes("ul > li") %>% html_text2 %>% str_subset("^Funktion: ") %>% str_trim("both")
          )
          
          (vb_gov_pos <- vb_persons[[k]] %>% html_nodes(".mod-common-revolving-door-data") %>% html_text2)
          if(length(vb_gov_pos) > 0) this_vb$vb_gov_pos <- vb_gov_pos
          
          vb_persons_df <- bind_rows(vb_persons_df, this_vb)
          
        }

        if(nrow(vb_persons_df) > 0) {
          vb_persons_df <- cbind(vb_persons_df, select(this_auftraggeber, c("file", "auftrag_id", "auftraggeber_id")))
          auftraggeber_people_panel <- bind_rows(auftraggeber_people_panel, vb_persons_df)
        }
        
        auftraggeber_panel <- bind_rows(auftraggeber_panel, this_auftraggeber)
      
        
      }
    }
    
   }

  #################
  # OLD
  #################
    if(length(myhtml %>% html_nodes(".client-organizations")) != 0) {  
      
    ###############
    # AUFTRAGGEBER
    (auftraggeber <- myhtml %>% html_nodes(".client-organizations > li"))
  
    if(length(auftraggeber) > 0) for (j in 1:length(auftraggeber)) {
      
      this_auftraggeber <- data.frame(file = file,
                                      auftrag_id = 0,
                                      auftraggeber_id = j,
                                      page_type = "old"
                                      )
      
      
      (auftraggeber_name <- auftraggeber[[j]] %>% html_nodes(".info-title a") %>% html_text %>% str_trim("both"))
      if(length(auftraggeber_name) == 1) if(auftraggeber[[j]] %>% html_nodes(".info-title a") %>% html_attrs %>% str_detect("encr") %>% any %>% suppressWarnings) (auftraggeber_name <- auftraggeber_name %>% unscrambler)
      
      if(length(auftraggeber_name) > 1 | length(auftraggeber_name) == 0) (auftraggeber_name <- auftraggeber[[j]] %>% html_nodes(".info-title .keep-spaces") %>% html_text  %>% str_trim("both"))
      if(length(auftraggeber_name) == 1) if(auftraggeber[[j]] %>% html_nodes(".info-title .keep-spaces") %>% html_attrs %>% str_detect("encr") %>% any %>% suppressWarnings) (auftraggeber_name <- auftraggeber_name %>% unscrambler)
      
     
      
      if(length(auftraggeber_name) > 0) this_auftraggeber$auftraggeber_name <- auftraggeber_name
      
      # Org type
      (auftraggeber_org_type <- auftraggeber[[j]] %>% html_nodes("div.info-title > div") %>% html_text2 %>% str_trim("both"))
      if(length(auftraggeber_org_type) > 0) this_auftraggeber$org_type <- auftraggeber_org_type
      
      # Link
      (auftraggeber_link <- auftraggeber[[j]] %>% html_nodes(".info-title .keep-spaces") %>% html_attr("href"))
      if(length(auftraggeber_link) > 1) (auftraggeber_link <- auftraggeber[[j]] %>% html_nodes(".info-title a") %>% html_attr("href"))
      if(length(auftraggeber_link) > 0) this_auftraggeber$auftraggeber_link <- auftraggeber_link
      
      # Vertretungsb. Personen der Auftraggeber (could be nested)
      (vb_persons <- html_nodes(auftraggeber[[j]], "div > ul.row > li.info-element"))

      vb_persons_df <- data.frame()
      if(length(vb_persons) > 0) for (k in 1:length(vb_persons)) {
        # k = 1
        (vb_name <- vb_persons[k] %>% html_nodes("span > span") %>% html_text2)
        if(vb_persons[k] %>% html_nodes("span > span") %>% html_attrs %>% str_detect("encr") %>% any %>% suppressWarnings()) (vb_name <- unscrambler(vb_name) %>% unname)
        (vb_fun <- vb_persons[k] %>% html_nodes("ul > li:nth-child(2)") %>% html_text2 %>% str_trim("both"))
        if(length(vb_fun) > 1) (vb_fun <- vb_persons[k] %>% html_nodes("ul > li:nth-child(2) div") %>% html_text2 %>% str_trim("both"))

        this_vb <- data.frame( # 
          vb_id = k,
          vb_name = vb_name,
          vb_fun = vb_fun
        )
    
        vb_persons_df <- bind_rows(vb_persons_df, this_vb)
        
      }
      
      if(nrow(vb_persons_df) > 0) {
        vb_persons_df <- cbind(vb_persons_df, select(this_auftraggeber, c("file", "auftrag_id", "auftraggeber_id")))
        auftraggeber_people_panel <- bind_rows(auftraggeber_people_panel, vb_persons_df)
      }
      
      # if(nrow(betraute_personen) > 0) this_auftrag <- bind_cols(this_auftrag, betraute_personen)
      auftraggeber_panel <- bind_rows(auftraggeber_panel, this_auftraggeber)
    
    
  }
  
  
    }
  }
}


# lobby_panel <- merge(lobby_panel, exact_times, by = "file", all.x = T)
lobby_panel$yearly_update <- str_detect(lobby_panel$update_info, "jährliche")
lobby_panel$from <- str_extract(lobby_panel$update_info, "(?<=(vom|seit) )\\d{2}\\.\\d{2}\\.\\d{4}  \\d{2}\\:\\d{2}\\:\\d{2}") %>% as.POSIXct(tz = Sys.timezone(), format="%d.%m.%Y  %H:%M:%S")
lobby_panel$until <- str_extract(lobby_panel$update_info, "(?<=(bis) )\\d{2}\\.\\d{2}\\.\\d{4}  \\d{2}\\:\\d{2}\\:\\d{2}") %>% as.POSIXct(tz = Sys.timezone(), format="%d.%m.%Y  %H:%M:%S")

save(lobby_panel, file = "lobby_panel.rda")
# load("lobby_panel.rda")


# Add register_number
people_panel$register_number <- str_extract(people_panel$file, "(?<=versions/).*(?=/)")

people_panel$update_date[is.na(people_panel$update_date)] <- people_panel$first_date[is.na(people_panel$update_date)]
people_panel$name_id <- str_c(people_panel$register_number, people_panel$name)  %>% str_replace_all("  {1,5}", " ") %>% str_replace_all(" {1,5}", " ")

# Add dates
# lobby_panel %>% names
people_panel <- merge(select(lobby_panel, c(file, update_date, first_date, org_name, inactive_date, from_time)), people_panel, by = "file", all = T)

people_panel <- filter(people_panel, !is.na(name))

###################################
# Name cleaning
###################################
# Use previous data from the lobby register to clean names for matching

people_panel$name_clean <- str_replace_all(people_panel$name, " {1,10}", " ")

load("json/lobbyists_from_json_2024-01-02_09-59-40.rda")
load("json/ver_persons_from_json_2024-01-02_09-59-40.rda")

remove_before <- (c(lobbyists$academicDegreeBefore, ver_persons$academicDegreeBefore) %>% unique)[-1]
remove_after <- (c(lobbyists$academicDegreeAfter, ver_persons$academicDegreeAfter) %>% unique)[-1]

remove_before <- remove_before[!(remove_before %in% c("Wohlrabe", "Kerstin ", "Jana", "Henry Calvert", "Anke", "Joachim", "Susann", "Ingo"))]
remove_after <- remove_after[!(remove_after %in% c("Volker", "Yvonne", "Dr. Andreas", "Alexander", "Jörg", "Hermann", "Dr. Peter", "Ingo"))]

remove_before <- remove_before[order(nchar(remove_before), decreasing = T)]
remove_after <- remove_after[order(nchar(remove_after), decreasing = T)]

people_panel$name_clean <- people_panel$name_clean %>% str_remove_all(str_c("^(", str_c(remove_before, collapse = "|"), ") ") %>% str_replace_all("\\.", "\\."))
people_panel$name_clean <- people_panel$name_clean %>% str_remove_all(str_c("^(", str_c(remove_before, collapse = "|"), ") ") %>% str_replace_all("\\.", "\\."))

people_panel$name_clean <- people_panel$name_clean %>% str_remove_all(str_c(" (", str_c(remove_after, collapse = "|") %>% str_replace_all("\\.", "\\."), ")$"))
people_panel$name_clean <- people_panel$name_clean %>% str_remove_all(str_c(" (", str_c(remove_after, collapse = "|") %>% str_replace_all("\\.", "\\."), ")$"))

people_panel$name_clean <- people_panel$name_clean %>% str_remove_all(" \\(.*\\)")

people_panel$name_clean <- people_panel$name_clean %>% str_replace_all(c("\\. Ingle" = "Shefalika Ingle", "\\. Heinemann" = "Britta Heinemann", "\\. Henning" = "Rita Henning", "\\. Dülk" = "Christian Dülk"))

save(people_panel, file = "people_panel.rda")
load("people_panel.rda")

# Auftraggeber are sometimes nested directly in lobby orgs (old) or can be nested within auftrage (new)
save(auftraggeber_panel, file = "auftraggeber_panel.rda")
load("auftraggeber_panel.rda")

# Nested in Auftraggeber
save(auftraggeber_people_panel, file = "auftraggeber_people_panel.rda")
load("auftraggeber_people_panel.rda")

table(auftraggeber_people_panel$vb_name %in% people_panel$name) 
table(str_c(auftraggeber_people_panel$vb_name, auftraggeber_people_panel$file) %in% str_c(people_panel$name, people_panel$file))
# Many of the people listed as vertretungsberechtigte personen of an auftraggeber are also in the register under a different ID/org

# Nested in Lobby orgs
auftrage_panel <- merge(auftrage_panel, select(lobby_panel, c("file", "first_date", "update_date")), by = "file", all.x = T)
save(auftrage_panel, file = "auftrage_panel.rda")
load("auftrage_panel.rda")

# Nested in Auftrage
save(auftrage_people_panel, file = "auftrage_people_panel.rda")
load("auftrage_people_panel.rda")





##############################
# EVALUATE DATA CONSISTENCY
##############################


# Lobbyregister as of 2 January 2024
load("json/lobbyists_from_json_2024-01-02_09-59-40.rda")

lobbyists$name <- str_c(lobbyists$commonFirstName %>% str_trim("both"), " ", lobbyists$lastName %>% str_trim("both"))
lobbyists$name[!is.na(lobbyists$academicDegreeBefore)] <- str_c(lobbyists$academicDegreeBefore[!is.na(lobbyists$academicDegreeBefore)] %>% str_trim("both"), " ", lobbyists$name[!is.na(lobbyists$academicDegreeBefore)])
lobbyists$name[!is.na(lobbyists$academicDegreeAfter)] <- str_c(lobbyists$name[!is.na(lobbyists$academicDegreeAfter)], " ", lobbyists$academicDegreeAfter[!is.na(lobbyists$academicDegreeAfter)] %>% str_trim("both"))
lobbyists$name <- str_trim(lobbyists$name, "both")

lobbyists$name_id <- str_c(lobbyists$registerNumber, lobbyists$name) %>% str_replace_all("  {1,5}", " ") %>% str_replace_all(" {1,5}", " ")

# filetime <- as.POSIXct("2024-01-02 09-59-40", tz = Sys.timezone(), format="%Y-%m-%d %H-%M-%S")

# Scraped panel
load("people_panel.rda")

table(people_panel$fun == "lobbyist")

people_panel$name_id %>% unique %>% length # 50,980 unique name + organization pairs
filter(people_panel, fun == "lobbyist")$name_id %>% unique %>% length # lobbyists: 34,051 unique name + organization pairs
filter(people_panel, fun != "lobbyist")$name_id %>% unique %>% length # others: 27,253 unique name + organization pairs (there are overlaps)

# Make a dataset that is comparable to the json
compare_panel <- people_panel %>% 
  filter(update_date < ymd("2024-01-03")) %>%  # Exclude updates after 2 January
  filter(is.na(inactive_date) | inactive_date >= ymd("2024-01-03")) %>% 
  group_by(register_number) %>% 
  filter(update_date == max(update_date)) %>% 
  filter(!is.na(fun)) %>%
  filter(fun == "lobbyist") %>%
# Only keep the most recent (after filtering)
  group_by(register_number, name) %>% 
  filter(row_number() == 1 & !is.na(name))

compare_panel$name_id <- str_c(compare_panel$register_number, compare_panel$name)  %>% str_replace_all("  {1,5}", " ") %>% str_replace_all(" {1,5}", " ")
table(compare_panel$name_id %in% lobbyists$name_id) # 89
# This means more people, which is better than people missing

# People that are in the html but not in the json data
# compare_panel[!(compare_panel$name_id %in% lobbyists$name_id), ] %>% View
# There are 9 people from updates from 2024-01-02, so they might have plausibly been added after the json data was scraped
# Others might be from corrections on the same day as updates were made

# Other way: People that are in the json data but not in the html
lobbyists[!(lobbyists$name_id %in% compare_panel$name_id), ] %>% View
table(lobbyists$name_id %in% compare_panel$name_id) # 10

lobbyists[!(lobbyists$name_id %in% compare_panel$name_id), "name_id"] %in% people_panel$name_id # All 10 are in the data! So no one is missing.

nrow(compare_panel) # 14,504
nrow(lobbyists) # 14,448
# Difference: 56


# Vertr.b. Persons

load("json/ver_persons_from_json_2024-01-02_09-59-40.rda")
nrow(ver_persons) # There are 18,891 names of vertretungsberechtigte personen in the register
ver_persons$name <- str_c(ver_persons$commonFirstName %>% str_trim("both"), " ", ver_persons$lastName %>% str_trim("both"))
ver_persons$name[!is.na(ver_persons$academicDegreeBefore)] <- str_c(ver_persons$academicDegreeBefore[!is.na(ver_persons$academicDegreeBefore)] %>% str_trim("both"), " ", ver_persons$name[!is.na(ver_persons$academicDegreeBefore)])
ver_persons$name[!is.na(ver_persons$academicDegreeAfter)] <- str_c(ver_persons$name[!is.na(ver_persons$academicDegreeAfter)], " ", ver_persons$academicDegreeAfter[!is.na(ver_persons$academicDegreeAfter)] %>% str_trim("both"))
ver_persons$name <- str_trim(ver_persons$name, "both")

ver_persons$name_id <- str_c(ver_persons$registerNumber, ver_persons$name) %>% str_replace_all("  {1,5}", " ") %>% str_replace_all(" {1,5}", " ")


compare_panel <- people_panel %>% 
  filter(update_date < ymd("2024-01-03")) %>%  # Exclude updates after 2 January
  filter(is.na(inactive_date) | inactive_date > ymd("2024-01-03")) %>% 
  group_by(register_number) %>% 
  filter(update_date == max(update_date)) %>% 
  filter(!is.na(fun)) %>%
  filter(fun != "lobbyist") %>%
  # Only keep the most recent (after filtering)
  group_by(register_number, name) %>% 
  filter(row_number() == 1 & !is.na(name))






compare_panel$name_id <- str_c(compare_panel$register_number, compare_panel$name)  %>% str_replace_all("  {1,5}", " ") %>% str_replace_all(" {1,5}", " ")
table(compare_panel$name_id %in% ver_persons$name_id) # 123

# People that are in the html but not in the json data
# compare_panel[!(compare_panel$name_id %in% ver_persons$name_id), ] %>% View

# Other way: People that are in the json data but not in the html
# ver_persons[!(ver_persons$name_id %in% compare_panel$name_id), ] %>% View
table(ver_persons$name_id %in% compare_panel$name_id) # 24

nrow(compare_panel) # 18,569
nrow(ver_persons) # 18,891
# Difference: 322



## 
# CHECK AUFTRAGGEBER PERSONS

# Are there persons listed in the auftraggeber data that are not in the people panel?
table(auftraggeber_people_panel$vb_name %in% people_panel$name) # 10,405 new names. These seem new!

# Show the new names
auftrage_people_panel$sub_name <- str_remove_all(auftrage_people_panel$sub_name, " \n        \n            .*$") %>% str_replace_all("  {1,5}", " ") %>% str_replace_all(" {1,5}", " ") %>% str_trim("both")
table(auftrage_people_panel$sub_name[is.na(auftrage_people_panel$sub_is_person) | auftrage_people_panel$sub_is_person] %in% c(people_panel$name, people_panel$org_name)) # 4 new names. These seem to be errors. All auftrage_people are in the people_panel

# filter(auftrage_people_panel, str_detect(sub_name, "Highgate")
auftrage_people_panel[!(auftrage_people_panel$sub_name %in% c(people_panel$name, people_panel$org_name)) & (is.na(auftrage_people_panel$sub_is_person) | auftrage_people_panel$sub_is_person), ] %>% View












current_panel <- people_panel %>% 
  group_by(register_number) %>% 
  filter(update_date == max(update_date)) %>% 
  filter(!is.na(fun)) %>%
  filter(fun == "lobbyist") %>% 
  group_by(register_number, name) %>% 
  filter(row_number() == 1 & !is.na(name))






##############################
# ARCHIVE
##############################


lobby_panel$from_time <- format(lobby_panel$from, format = "%H:%M:%S")
lobby_panel$from_time <- str_c("1970-01-01 ", lobby_panel$from_time) %>%  as.POSIXct(tz = Sys.timezone(), format="%Y-%m-%d %H:%M:%S")

# Show the lunch break
ggplot(lobby_panel, aes(from_time)) +
  geom_histogram(bins = 200)
# NEW

# file = "versions/R006795/29509.html"
# file = "versions/R000015/34265.html" # w auftraggeber
# file = "versions/R006653/37468.html" #  w exposed position
# file = "versions/R006795/29509.html" # w finanzmittel fuer auftraggeber
# file = "versions/R006791/34076.html" # w inactive status
# file = "versions/R000014/9887.html" # old?
# file = "versions/R000113/29846.html" # special case for vb_persons
# file = "versions/R002341/41058.html" # w unterauftragnehmer
# file = "versions/R006838/33532.html" # many auftrage, gov_pos at highest level
# file = "versions/R003434/41769.html" # gov_pos im auftrag
# file = "versions/R000343/15022.html" # Many auftraggeber


# OLD

# file = "versions/R000015/1212.html"

# file = "versions/R000012/5.html" # OLD?
# file = "versions/R000014/9887.html" # old?
# file= "versions/R000001/14614.html"
# file= "versions/R000015/10975.html"
# file = "versions/R000343/15022.html" # Many auftraggeber
# file = "versions/R000014/30671.html" # Link in auftrage


# file = "versions/R001007/3061.html"
# file = "versions/R000861/23802.html"
# file = "versions/R000015/1212.html"
# file = "versions/R000015/34265.html"
# file = "versions/R006795/29509.html"

# file = "versions/R000366/29924.html"

# 2024-08-02
# file = "versions/R000366/29924.html" # missing org_type/link in auftrage
# file = "versions/R000758/38895.html" # error
# file = "versions/R000814/36064.html" # error
# file = "versions/R003447/1594.html" # old, encrypted auftraggeber name
# file = "versions/R001428/230.html"
# file = "versions/R001851/25307.html" ?
# file = "versions/R005558/27557.html" # normal inactive
# file = "versions/R004589/39822.html" # inactive weird
# file = "versions/R000001/14614.html"
# file = "versions/R001428/230.html"
# myhtml <- read_html(file, encoding = "UTF-8")



# lobbyists <- myhtml %>% html_nodes(".employees-entrusted-persons .mod-common-search-person-name") %>% html_text() %>% str_trim("both") # New htmls
# 
# 
# if(length(lobbyists) == 0) lobbyists <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-identity-finances ol li span.mod-common-search-person-name") %>% html_text() %>% str_trim("both") # Old htmls
# 
# if(length(ver_persons) == 0 & length(lobbyists) == 0) people <- data.frame()
# 
# if(length(ver_persons) > 0) people <- data.frame(name = ver_persons[which(str_detect(ver_persons, "Funktion: ")) - 1],
#                                                  role = ver_persons[which(str_detect(ver_persons, "Funktion: "))] %>% str_remove("Funktion: "),
#                                                  number = 1:length(ver_persons[which(str_detect(ver_persons, "Funktion: ")) - 1]))
# 
# if(length(lobbyists) > 0)  people <- data.frame(name = lobbyists,
#                                                 role = "lobbyist",
#                                                 number = 1:length(lobbyists)) %>% bind_rows(people, .)
# 
# if(myhtml %>% html_nodes(".mod-common-search-person-name span") %>% html_attrs %>% str_detect("encrypted") %>% any %>% suppressWarnings()) people$name <- people$name %>% sapply(unscrambler)
# 
# 
# 
# 
# 
# 
# (funding_sources <- myhtml %>% html_nodes(".main-funding-sources") %>% html_text2)
# if(length(funding_sources) == 0) funding_sources <- NA
# # lobby_money <- (myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-identity-finances ul li:nth-child(1)") %>% html_text)[1] %>% str_remove("J.*hrliche finanzielle Aufwendungen im Bereich der Interessenvertretung:") %>% str_trim("both") 
# # lobby_people <- (myhtml %>% html_nodes(".mod-common-search-register-entry-content-information-identity-finances ul li:nth-child(2)") %>% html_text)[1] %>% str_remove("Anzahl der Besch.*ftigten im Bereich der Interessenvertretung:") %>% str_trim("both")
# 
# 
# # "versions/R000015/32181.html"
# org_name <- myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title h2") %>% html_text2
# if(myhtml %>% html_nodes(".mod-common-search-register-entry-content-header-title span") %>% html_attrs %>% str_detect("encrypted") %>% any %>% suppressWarnings()) org_name <- org_name %>% unscrambler %>% str_trim("both")
# 
# 
# mynodes <- myhtml %>% html_nodes(".info-wrapper")
# update_date <- mynodes[mynodes %>% as.character %>% str_detect("Letzte Änderung")] %>% html_nodes(".element-lg-block") %>% html_text() %>% dmy
# first_date <- mynodes[mynodes %>% as.character %>% str_detect("Erst")] %>% html_nodes(".element-lg-block") %>% html_text() %>% dmy
# 
# thisentry <- data.frame(org_name = org_name,
#                         register_number = dir %>% str_extract("(?<=/).*"), 
#                         version_id = file %>% str_extract("(?<=/)[:digit:]*(?=\\.html$)"),
#                         update_date = update_date, 
#                         first_date = first_date, 
#                         file = file, 
#                         lobby_money = lobby_money, 
#                         lobby_people = lobby_people,
#                         funding_sources = funding_sources) 
# 
# if (nrow(people) > 0) thisentry <- cbind(thisentry, people)
# 
# 
# ##############################
# # MAKE MORE VARS
# ##############################
# 
# 
# # people_panel$lobbyist_name <- people_panel$lobbyist_name %>%  str_replace_all(c("Ä±" = "ı", "  " = " ", "Å\u009f" = "ş", "Ã¼" = "ü", "Ã¶" = "ö", "â¬" = "€", "Ã¤" = "ä", "Ã\u009f" = "ß", "Ã\u0084" = "Ä", "Ã\u0096" = "Ö", "Ã\u009c" = "Ü", "Â§" = "§", "Ã©" = "é", "Ã¡" = "á", "Ã±"="ñ", "Ã³" = "ó", "Ã" = "í", "í\u0089" = "É"))
# 
# # people_panel$lobby_money <- people_panel$lobby_money %>%  str_replace_all(c("Ä±" = "ı", "  " = " ", "Å\u009f" = "ş", "Ã¼" = "ü", "Ã¶" = "ö", "â¬" = "€", "Ã¤" = "ä", "Ã\u009f" = "ß", "Ã\u0084" = "Ä", "Ã\u0096" = "Ö", "Ã\u009c" = "Ü", "Â§" = "§", "Ã©" = "é", "Ã¡" = "á", "Ã±"="ñ", "Ã³" = "ó", "Ã" = "í", "í\u0089" = "É"))
# 
# # people_panel$lobbyist_name[str_detect(people_panel$lobbyist_name , "Pénz") & !is.na(people_panel$lobbyist_name)]
# 
# # Add fiscal year
# people_panel$year <- people_panel$lobby_money %>% str_extract("Geschäftsjahr:.*") %>% str_remove("Geschäftsjahr: ")
# table(people_panel$year)
# people_panel$fiscal_year <- NA
# people_panel$fiscal_year[str_detect(people_panel$year, "^01/20")] <- 2020
# people_panel$fiscal_year[str_detect(people_panel$year, "^01/21")] <- 2021
# people_panel$fiscal_year[str_detect(people_panel$year, "^01/22")] <- 2022
# people_panel$fiscal_year[str_detect(people_panel$year, "^01/23")] <- 2023
# 
# table(people_panel$fiscal_year)
# 
# people_panel$lobbying_amount_est <- people_panel$lobby_money %>% str_replace_all(c("\r" = " ", "\t" = " ", "\n" = " ")) %>%  str_extract("(?<=bis \\d\\d\\/\\d\\d).*") %>%  str_extract_all("(\\d{1,3}\\.)?(\\d{1,3}\\.)?(\\d{1,3}\\.)?\\d{1,3}", simplify = T)
# 
# # Take the the center of the interval as the value
# people_panel$lobbying_amount_est <- (people_panel$lobbying_amount_est[, 1] %>% str_remove_all("\\.") %>%  as.numeric() - 1 + people_panel$lobbying_amount_est[, 2] %>% str_remove_all("\\.") %>%  as.numeric())/2
# 
# # Fix zeros
# people_panel$lobbying_amount_est[str_detect(people_panel$lobby_money %>% str_replace_all(c("\r" = " ", "\t" = " ", "\n" = " ")), "/\\d\\d *0 Euro")] <- 0
# 
# # Do the same for the number of people (in intervals)
# people_panel$people_est <- people_panel$lobby_people %>% str_extract("Anzahl der Beschäftigten im Bereich der Interessenvertretung:(\n|\r|\t<|.)*") %>%  str_extract_all("\\d{1,4}", simplify = T)
# people_panel$people_est <- (people_panel$people_est[, 1] %>% str_remove_all("\\.") %>%  as.numeric() - 1 + people_panel$people_est[, 2] %>% str_remove_all("\\.") %>%  as.numeric())/2
# people_panel$people_est[people_panel$lobby_people == "Anzahl der Beschäftigten im Bereich der Interessenvertretung:\n0"] <- 0
# 
# people_panel$lobby_pos <- people_panel$lobby_people %>% str_extract("Vollzeitäquivalent der im Bereich der Interessenvertretung beschäftigten Personen:(\n|\r|\t<|.)*") %>%  str_extract_all("\\d{1,4},\\d{1,4}", simplify = T) %>% str_replace(",", ".") %>%  as.numeric()
# 
# 
# # Count number of named lobbyists
# people_panel <- people_panel %>% group_by(register_number, version_id) %>% mutate(named_lobbyists = n()) 
#                                                                                       
# people_panel <- people_panel %>% dplyr::group_by(register_number, version_id) %>% dplyr::mutate(named_lobbyists = ifelse(test = all(is.na(lobbyist_name)), yes = 0, no = named_lobbyists))
# 
# people_panel <- unique(people_panel)
# 
# 
# lobbyist_names$duplicated <- lobbyist_names %>% select(register_number, name) %>% duplicated(fromLast = T)
# 
# 
# people_panel$update_date[is.na(people_panel$update_date)] <- people_panel$first_date[is.na(people_panel$update_date)]
# 
# 
# 
# save(people_panel, file = "people_panel.rda")
# 
# 
# 
# 
# ##############################
# # AGGREGATE THE DATA TO ONE ROW PER ORG AND YEAR
# ##############################
# 
# 
# 
# # Here we need to distinguish between the estimated expenditures for the fiscal_year, and the people info for the update_date
# # people_panel_agg <- aggregate(cbind(lobbying_amount_est, people_est, named_lobbyists) ~ register_number + fiscal_year, data = people_panel_agg, FUN = max) # Old version
# 
# people_panel_agg_money <- unique(select(people_panel, c(register_number, fiscal_year, lobbying_amount_est)))
# people_panel_agg_money <- aggregate(cbind(lobbying_amount_est) ~ register_number + fiscal_year, data = people_panel_agg_money, FUN = max)
# 
# 
# people_panel_agg_people <- unique(select(people_panel, c(register_number, update_date, people_est, named_lobbyists)))
# people_panel_agg_people$fiscal_year <- people_panel_agg_people$update_date %>% str_sub(1, 4) %>% as.numeric()
# people_panel_agg_people <- aggregate(cbind(people_est, named_lobbyists) ~ register_number + fiscal_year, data = people_panel_agg_people, FUN = max)
# 
# 
# people_panel_agg <- merge(people_panel_agg_money, people_panel_agg_people, by = c("fiscal_year", "register_number"), all = T)
# 
# save(people_panel_agg, file = "people_panel_agg.rda")
# 
# # origin is sometimes scrambled! Use unscrambler if necessary (detect scrambled data by too many consonants?)
# money_lobby <- merge(people_panel_agg, money_panel_agg, by = c("fiscal_year", "register_number"), all = T)
# 
# ##############################
# # ADD LOBBY ORG TYPE FROM LOBBYREGISTER DATA
# ##############################
# 
# # Get list of lobby orgs
# lobbylist <- "Lobbyregistersuche-Details-2024-01-02_09-59-40.json" %>% # "Lobbyregistersuche-Details-2023-04-20_11-17-27.json" %>% 
#   fromJSON
# 
# # Extract type
# acitvities <- data.frame(activitiy_de = lobbylist$results$registerEntryDetail$activity$de, activity_en = lobbylist$results$registerEntryDetail$activity$en, register_number = lobbylist$results$registerNumber)
# 
# dim(money_lobby)
# 
# # Merge
# money_lobby <- merge(money_lobby, acitvities, all.x = T, by = "register_number")
# 
# dim(money_lobby)
# 
# # Add abbreviation of type
# money_lobby$activity_combined <- money_lobby$activity_en %>% recode( "Science, think tank, research institution or college" = "Science", 
#                                                                      "Organization under private law with public welfare tasks (e.g. registered associations, foundations)" = "Publ. welfare",
#                                                                      "Non-governmental organizations (non-governmental organization, platform or network)" = "NGO",
#                                                                      "Church or other religious or ideological community" = "Other",
#                                                                      "Consulting company, independent consultant" = "Consultancy",
#                                                                      "Public law organization (e.g. corporations, institutions and foundations under public law)" = "Other",
#                                                                      "Law firm, individual attorney" = "Other",
#                                                                      "Workers association" = "Other",
#                                                                      "Professional association" = "Prof. assn.",
#                                                                      "Employers association" = "Employers",
#                                                                      "Trade association" = "Trade assn.")
# 
# save(money_lobby, file = "money_lobby.rda")
# 
# 
# 
# 
# 
# 
# ###### ARCHIVE  UNTERAUFTRAGNEHMER
# 
# # UNTERAUFTRAGNEHMER
# (unterauftragnehmer <- auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors  ol > li "))) # 
# 
# # unterauftragnehmer[[1]][2] %>% html_text2
# 
# for (l in 1:length(unterauftragnehmer[[1]])){
#   
#   (unterauftragn <- unterauftragnehmer[[1]][l])
#   # name
#   unterauftragn %>% html_nodes("div.info-title > span") %>% html_text2
#   # link
#   # org_type
#   
#   # vb
#   
#   # eingesetzte personen
# }
# 
# 
# 
# # Unterauftragnehmer
# unterauftragnehmer <- data.frame(
#   sub_name = auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors div.info-title") %>% html_text2) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist,
#   sub_link = auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors a") %>% html_attr("href")) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist,
#   sub_type = "subcontractor"
# ) 
# 
# # "versions/R003046/32028.html"
# 
# # sub_name
# auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors div.info-title") %>% html_text2) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist
# 
# # sub_link
# auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors a") %>% html_attr("href")) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist
# 
# (sub_is_person <- (auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors li")))[[1]] %>% lapply(function (x) html_nodes(x, ".mod-common-search-person-name")) %>% lapply(function (x) length(x)) %>% as.logical)
# if(length(sub_is_person) > 0) (unterauftragnehmer$sub_is_person <- sub_is_person)
# unterauftragnehmer <- unterauftragnehmer %>% filter(!is.na(sub_name) | !is.na(sub_link))
# 
# # eingesetzte personen
# 
# # is_person <- 
# # auftrag %>% list %>% lapply(function (x) html_nodes(x, ".contract-contractors a") %>% html_attr("href")) %>% lapply(function (x) if(length(x) == 0) NA else x) %>% unlist
# 


