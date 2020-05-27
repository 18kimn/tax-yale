#cleaning property data -- extracting which variables we need
#these loops take a bit of time to run... it probably wasn't the best practice to save every property 
#to a different file while scraping. I did so because I was worried about losing internet access or some other interruptions, which would mean trying to do everything in one go would be hard. 
library(tidyverse)

basic <- readRDS("data/raw/property/full_df.RDS") %>% 
  select(pid, year_built = `Year Built:`, owner, address, 
         rcmdepreciation) %>% 
  mutate(year_built = ifelse(year_built == "", NaN, as.numeric(year_built)),
         rcmdepreciation = as.numeric(str_remove_all(rcmdepreciation, "\\$|,"))) %>% 
  mutate_at(vars(owner, address), str_trim)

assessment <- map_dfr(list.files("data/raw/property/valuations/",
                             full.names = T), 
function(filename){
  pid <- str_remove(str_remove(filename, 
                               "data/raw/property/valuations/"),
                    ".RDS")
  #print(paste("Scraping", pid)) #was useful when writing code to see where something went wrong. annoying on execution
  
  df <- readRDS(filename) %>% 
    reduce(bind_rows) 
  if(str_detect(df[1,1], "No Data")) return(NULL)
  
  df <- df %>% 
    mutate(year = as.numeric(`Valuation Year`),
           pid = pid) %>% 
    arrange(desc(year)) %>% top_n(1, year)
  return(df)
})

assessment <- assessment %>% select(-`Valuation Year`, -year) %>% 
  mutate_at(vars(Improvements, Land, Total), 
            ~as.numeric(str_remove_all(., "\\$|,"))) 

areas <- map_dfr(list.files("data/raw/property/areas", 
                            full.names = T),
function(filename){
  pid <- str_extract(filename, "[[:digit:]]+")
  #print(paste("Scraping", pid)) 
 
  
  df <- readRDS(filename)
  
  if(str_detect(df[1,1], "No Data")) return(NULL)
  

  df <- df %>% 
    filter(Description == "") %>% #take the totals -- 
    select(GrossArea, LivingArea) %>%
    mutate(pid = pid) %>%
    mutate_at(vars(GrossArea, LivingArea), ~as.numeric(str_remove(., ",")))

  return(df)
})


full <- list(basic,areas, assessment) %>% 
  reduce(full_join, by = c("pid")) %>% 
  rename(improvement_value = Improvements, 
         land_value = Land,
         gross_area = GrossArea, 
         living_area = LivingArea) %>% 
  select(address, owner, year_built, everything(), -Total, -pid)

write_csv(full, "data/clean/property_info.csv")


#using below for Census Geocoding
full <- full %>% 
  mutate(`Unique ID` = 1:nrow(full), 
         City = "New Haven", 
         State = "CT",
         ZIP = "") %>% 
  select(`Unique ID`, `Street address` = address, 
         City, State, ZIP)


write_csv(full[1:9999,], "data/raw/property/merged (pre-geocoding)/property_info1.csv")
write_csv(full[10000:19998,], "data/raw/property/merged (pre-geocoding)/property_info2.csv")
write_csv(full[19999:nrow(full),], "data/raw/property/merged (pre-geocoding)/property_info3.csv")
