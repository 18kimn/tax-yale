library(tigris)
library(sf) 
library(tidyverse)
nhv <- tracts(state = "CT", county = "New Haven") %>% 
  st_as_sf() %>% #super picky formatting means I like a tibble inside of the sf object. Of no substantive importance. 
  tbl_df() %>% 
  st_as_sf()

saveRDS(nhv, "data/clean/nhv_shapefile.RDS")
#andddd yep that's it
#nathan 3/4/2020