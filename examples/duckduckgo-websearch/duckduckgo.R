duckducksearch <- function(SEARCHTERM){
  
  response <- GET(paste0("https://duckduckgo.com/?q=!ducky+", URLencode(SEARCHTERM), "&format=json"))
  RESULT <- response$url
  return(RESULT)
}