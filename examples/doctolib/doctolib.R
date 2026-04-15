# Load required libraries for HTTP requests, notifications, JSON handling, data manipulation, and date handling
library(httr)
library(beepr)
library(rjson)
library(dplyr)
library(lubridate)
library(stringr)

# Define a data frame with doctor information (IDs and motive IDs for booking)
doctors <- data.frame(
  id = c("seda-yayla-bozdag", "delano-pathirana-schoeneberg", "sergi-jebashvili", "gu-sophie", "alina-rosenberger", "ines-riani", "gokhan-mutlur", "anna-pokrywka"),
  # name = c("Frau Seda Yayla-Bozdag, Allg. Sprechstund, Breite Straße 17, 13597 Berlin",
           # "Dr. med. Delano Pathirana, Sprechstunde, Tauentzienstraße 1, 10789 Berlin"),
  # openurl = c("https://www.doctolib.de/hautarzt/berlin/seda-yayla-bozdag/booking/availabilities?pid=practice-21524&specialityId=1289&telehealth=false&placeId=practice-21524&insuranceSector=public&insuranceSectorEnabled=true&motiveIds%5B%5D=8263944",
              # "https://www.doctolib.de/hautarzt/berlin/delano-pathirana-schoeneberg/booking/availabilities?pid=practice-70499&specialityId=1289&telehealth=false&placeId=practice-70499&insuranceSector=public&insuranceSectorEnabled=true&motiveIds%5B%5D=1085626"),
  motive_ids = c("8263944", "1085626", "7001268", "8263976", "5977678", "841914", "318049", "434276") #,
  # practice_ids = c("21524", "70499", "339921"),
  # agenda_ids = c("54165", "180192", "1029953")
)

# Loop through each doctor to fetch agenda, name, and practice ID from Doctolib API
for (i in 1:nrow(doctors)) {
  getreply <- GET(str_c("https://www.doctolib.de/online_booking/draft/new.json?id=", doctors$id[i]))

  # Extract agenda ID, doctor's name, and practice ID from the API response
  doctors$agenda_ids[i] <- content(getreply)$data$agendas[[1]]$id
  doctors$name[i] <- str_c(content(getreply)$data$places[[1]]$name, " ", content(getreply)$data$practitioners[[1]]$name_with_title_and_determiner)
  doctors$practice_ids[i] <- content(getreply)$data$places[[1]]$practice_ids[[1]]
}

doctors$next_date <- NA  # Initialize column for next available date

start_date <- "2023-10-22"  # Set the start date for searching availabilities

# min_date <- NA

next_date <- "2025-10-22"   # Placeholder for next available date

# Main loop: continuously check for availabilities for each doctor
while(T) {

  for (i in 1:nrow(doctors)) {
    
    print(doctors$name[i])  # Print the doctor's name
    
    # Construct the URL for checking availabilities
    geturl <- str_c("https://www.doctolib.de/availabilities.json?start_date=", start_date, 
                    "&visit_motive_ids=", doctors$motive_ids[i], 
                    "&agenda_ids=", doctors$agenda_ids[i],
                    "&insurance_sector=public&practice_ids=", doctors$practice_ids[i], "&limit=4")
    
    getreply <- GET(geturl)  # Send GET request to Doctolib
    
    getreply %>% content     # Parse the response
    
    next_slot <- (getreply %>% content)$next_slot  # Extract the next available slot
    
    if(is.null(next_slot)) {
      # If no availability, print message and set next_date to NA
      next_slot <- ""
      cat("no availabilities           ", "  ", "\n")
      doctors$next_date[i] <- NA
    } else {
      # If there is an availability, extract and print the date
      doctors$next_date[i] <- next_slot %>% substr(1,10) %>% ymd()
      cat("( Availability, but on ", doctors$next_date[i] %>% as.Date %>% as.character, ")\n")
      
      # If the next available date is within a certain range, alert the user
      if(next_date >= ymd("2023-10-20") & next_date <= ymd("2023-10-23")) {
        beep(2)  # Play a sound notification
        cat("Avaialability! on ", as.character(doctors$next_date[i]), ")\n")
        browseURL(str_c("https://www.doctolib.de/hautarzt/berlin/", doctors$id[i]))  # Open the booking page
        # break
        Sys.sleep(20)  # Wait for 20 seconds
      }
    }
   
    # Print the next available date among all doctors
    cat("NEXT AVAILABLE = ", as.character(min(doctors$next_date, na.rm = T) %>% as.Date), "\n\n")
    Sys.sleep(runif(1, 4, 6))  # Wait for a random time between 4 and 6 seconds
    
  }
  
}

