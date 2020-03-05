#cleaning the scraped data
library(tidyverse)

#1: recode column names
#2: merge in select data from other files

base <- readRDS("full_df.RDS") %>% 
  pivot_longer(cols = address:Super) %>% 
  mutate(name = str_replace_all(name, "Xtra", "Extra") %>% 
           str_replace_all("rms", "rooms") %>% 
           str_replace_all("Fin ", "Final ") %>% 
           str_replace_all("Bsmnt", "Basement") %>% 
           str_remove_all(":") %>% 
           str_replace_all("Ttl", "Total") %>% 
           str_replace_all("Flr", "Floor") %>% 
           str_replace_all("Cmplx", "Complex") %>% 
           str_replace_all("Pctgood", "Building Percent Good") %>% 
           str_replace_all("_", " ") %>% 
           str_replace_all("Replacement Cost\r\n                    Less Depreciation", "Replacement Cost Minus Depreciation") %>% 
           str_replace_all("Rcmdepreciation", "Replacement Cost Minust Depreciation") %>% 
           str_replace_all("Bthrms|Baths|Bathrms", "Bathrooms") %>% 
           str_replace_all("Total Extra Fixtrs", "Extra Fixtrs") %>% 
           str_replace_all("Bldg", "Building") %>% 
           str_to_title())
           
