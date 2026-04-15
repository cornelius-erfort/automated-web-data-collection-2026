library(jsonlite)
library(tidyverse)

setwd("data")

# load("lobbyist_panel.rda")


lobbyregister <- readLines("json/Lobbyregistersuche-Details-2024-01-02_09-59-40.json")
# lobbyregister <- readLines("json/Lobbyregistersuche-Details-2023-04-20_11-17-27.json")
# lobbyregister <- readLines("json/Lobbyregistersuche-Details-2022-06-22_18-54-24.json")
# lobbyregister <- "json/obbyregistersuche-2024-07-25_15-47-07.json" %>% readLines

lobbyregister <- rjson::fromJSON(lobbyregister)
# lobbyregister$results[5374]
# lobbyregister$results[5374][[1]]$registerEntryDetail$financialExpensesEuro$from
# test <- lobbyregister$results[5374][[1]]
# test$registerEntryDetail$detailsPageUrl
# lobbyregister$results[55][[1]]$registerEntryDetail$financialExpensesEuro$from


# Lobbyist names
lobbyists <- data.frame()

for (i in 1:length(lobbyregister$results)) {

  
  if(length(lobbyregister$results[[i]]$registerEntryDetail$lobbyistIdentity$namedEmployees) == 0) next
  mynames <- lobbyregister$results[[i]]$registerEntryDetail$lobbyistIdentity$namedEmployees %>% bind_rows
  
  myrow <- lobbyregister$results[[i]]$registerEntryDetail
  
  myrow <- myrow[names(myrow) %in% c("id", "detailsPageUrl", "validFromDate", "codexViolation", "refuseFinancialExpensesInformation", "refusePublicAllowanceInformation", "refuseDonationInformation", "activityDescription", "activityOperationType")]
 
  
  lobbyists <- bind_rows(lobbyists,  cbind(mynames, data.frame(myrow, lobbyist_name = lobbyregister$results[[i]]$registerEntryDetail$lobbyistIdentity$name, activity_en = ifelse(is.null(lobbyregister$results[[i]]$registerEntryDetail$activity$en), NA, lobbyregister$results[[i]]$registerEntryDetail$activity$en), registerNumber = lobbyregister$results[[i]]$registerEntryDetail$account$registerNumber)))
  

}


nrow(lobbyists)

write.csv(lobbyists, "json/lobbyists_from_json_2024-01-02_09-59-40.csv")
save(lobbyists, file = "json/lobbyists_from_json_2024-01-02_09-59-40.rda")

# Do these names match the names in the scraped data?
# Get lobbyregister as of 2024-01-02

compare_panel <- lobbyist_panel %>% 
  filter(update_date <= ymd("2024-01-02")) %>% 
  filter(role == "lobbyist") %>% 
  group_by(register_number) %>% 
  filter(update_date == max(update_date)) %>% 
  group_by(register_number, name) %>% 
  filter(row_number() == 1  & !is.na(name))
dim(compare_panel)

lobbyists$name <- str_c(lobbyists$commonFirstName, " ", lobbyists$lastName)
lobbyists$name[!is.na(lobbyists$academicDegreeBefore)] <- str_c(lobbyists$academicDegreeBefore[!is.na(lobbyists$academicDegreeBefore)], " ", lobbyists$name[!is.na(lobbyists$academicDegreeBefore)])
lobbyists$name[!is.na(lobbyists$academicDegreeAfter)] <- str_c(lobbyists$name[!is.na(lobbyists$academicDegreeAfter)], " ", lobbyists$academicDegreeAfter[!is.na(lobbyists$academicDegreeAfter)])
lobbyists$name <- str_trim(lobbyists$name, "both")

table(compare_panel$name %in% lobbyists$name) # In HTML but not in JSON? Some +4k out of +14k. These could be entries that were already inactive at that time, as the JSON only includes active entries.
compare_panel$name[!(compare_panel$name %in% lobbyists$name)]


table(lobbyists$name %in% compare_panel$name) # In JSON but not in HTML? Only 64 out of +14k. But these people should be in the HTML. These might be from entries that were made right after the JSON was retrieved, but still on the same day.
lobbyists$name[!(lobbyists$name %in% compare_panel$name)]

lobbyists %>% filter(!(name %in% compare_panel$name)) %>% View


(lobbyists$academicDegreeBefore %>% table %>% sort(decreasing = T))[1:300]
(lobbyists$academicDegreeAfter %>% table %>% sort(decreasing = T))[1:200]

# Vertretungsberechtigte Personen

lobbyregister$results[[i]]$registerEntryDetail$lobbyistIdentity$legalRepresentatives 


ver_persons <- data.frame()

for (i in 1:length(lobbyregister$results)) {
  
  
  if(length(lobbyregister$results[[i]]$registerEntryDetail$lobbyistIdentity$legalRepresentatives) == 0) next
  mynames <- lobbyregister$results[[i]]$registerEntryDetail$lobbyistIdentity$legalRepresentatives %>% bind_rows
  
  myrow <- lobbyregister$results[[i]]$registerEntryDetail
  
  myrow <- myrow[names(myrow) %in% c("id", "detailsPageUrl", "validFromDate", "codexViolation", "refuseFinancialExpensesInformation", "refusePublicAllowanceInformation", "refuseDonationInformation", "activityDescription", "activityOperationType")]
  
  
  ver_persons <- bind_rows(ver_persons,  cbind(mynames, data.frame(myrow, lobbyist_name = lobbyregister$results[[i]]$registerEntryDetail$lobbyistIdentity$name, activity_en = ifelse(is.null(lobbyregister$results[[i]]$registerEntryDetail$activity$en), NA, lobbyregister$results[[i]]$registerEntryDetail$activity$en), registerNumber = lobbyregister$results[[i]]$registerEntryDetail$account$registerNumber)))
  
  
}


nrow(ver_persons)

write.csv(ver_persons, "json/ver_persons_from_json_2024-01-02_09-59-40.csv")
save(ver_persons, file = "json/ver_persons_from_json_2024-01-02_09-59-40.rda")
load("json/ver_persons_from_json_2024-01-02_09-59-40.rda")

