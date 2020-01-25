#geocoding using the Texas A&M
#This is a little hacky (we have to construct our own API calls), but traditional geocoding services are a tad pricey -- for Google Maps, it would have taken ~$160 for $135 addresses. The most popular free one, OpenStreetMap/Nominatim, seems to incorrectly locate addresses of interest. 

#In comparison, the Texas A&M API is a bit less flexible, much slower, 
library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)

yale <- readRDS("full_df.RDS")  %>% filter(str_detect(owner, "YALE UNIVERSITY"), owner != "CHABAD AT YALE UNIVERSITY INC")
api_key <- "defccb796c34447fbfeb512590482e0c"
#john we have to remember to make that private if we make the github repo public

raw_url <- "https://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?"

paulimurray <- GET(url = raw_url, query = list(apiKey = api_key, version = "4.01", 
    streetAddress = "70 Sachem Street", city = "New Haven", 
    state = "CT"))

geocode <- function(address){
  raw_result <- GET(url = raw_url, query = list(apiKey = api_key, version = "4.01", 
                                  streetAddress = address, city = "New Haven", 
                                  state = "CT"))$content %>% 
    rawToChar()
  latlong <- raw_result  %>% 
    str_split(",") %>% 
    unlist %>% 
    extract(4:5)
  coords <- tribble(~address, ~lat, ~long, 
          address, latlong[1], latlong[2])
  print(paste("Geocoded", address))
  saveRDS(coords, paste0("geocoded/", address, ".RDS"))
  return(coords) 
}



yaleaddresses <- map_dfr(yale$address, geocode)
saveRDS(yaleaddresses, "yaleaddresses.RDS")
