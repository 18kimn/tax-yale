#mapmakingggg yayyyy
library(leaflet)
library(sf)
library(tidyverse)
library(osmdata)
library(magrittr) #pipe-friendly "[" and "[[" operators]
#wow, i haven't done this in a while -- kind of excited
#goals: interactive map. hovering over it reveals three main things: name of building, its property value, 
#and the corresponding amount of taxes that ylae does not pay

#as of 3/14 we didn't make a column for property taxes yet, but that should be pretty easy right? 
#really all we need for our map is the address, the total assessment, and the long/lat
#I decided to get the building outliines from OSM because...because lol idk

dt <- read_csv("data/OPM data/opm clean.csv") %>% 
  select(addr = Address_out, tot = Total.Assessment) %>% 
  drop_na(addr) %>% 
  group_by(addr) %>% 
  summarise(tot = sum(tot))

clean_street <- function(st){
  str_replace_all(st, "( St$)|( St )", " Street ") %>% 
    str_replace_all(" Ave$", " Avenue") %>% 
    str_replace_all(" Pl$", " Place") %>% 
    str_replace_all(" Pkway", " Parkway") %>% 
    str_replace_all(" Dr$", " Drive") %>% 
    str_replace_all(" Sq$", " Square")
}

dt <- dt %>% 
  separate(addr, into = c("addr.housenumber", "addr"), sep = c(" "), extra = "merge") %>% 
  separate(addr, into = c("addr.street", "addr.city", "addr.state", "addr.postcode"), sep = ",") %>% 
  mutate(addr.street = clean_street(addr.street)) %>% 
  mutate_at(vars(addr.street, addr.city), function(x) str_to_title(str_trim(x))) %>% 
  select(-addr.state, -addr.postcode)

#OSM starts here
osm_dt <- getbb("New Haven, Connecticut") %>% 
  opq() %>% add_osm_feature(key = "building") %>% 
  osmdata_sf() 

#really hope the joining works
joined <- inner_join(dt, osm_dt, by = c("addr.housenumber", "addr.street", "addr.city")) %>% 
  mutate(addr = paste(addr.housenumber, addr.street))
new_dt <- dt %>% 
  filter(!(paste(addr.housenumber, addr.street) %in% joined$addr))

#extracting relevant data and binding it together
osm_dt <- osm_dt %>% 
  extract(c("osm_polygons", "osm_multipolygons")) %>% 
  map(function(df) select(df, "name", contains("addr") & !one_of("addr.interpolation", "addr.country"))) %>% 
  reduce(rbind)



#formatting our data into 







dt %>% leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~long, ~lat, popup = ~Address_out, 
             radius = 1) %>% 
  extract(c(""))
  
