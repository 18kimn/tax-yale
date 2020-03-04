#contextual variables to be used with the property data
library(tidyverse)
library(tidycensus)

acs_vars <- load_variables(2018, "acs5")

vars_tbl <- acs_vars %>% 
  filter(str_detect(name, "C15002|B25003|B03002|B19113|C27001|B07004")) %>% 
  filter((str_detect(name, "C15002") & str_detect(name, "_002|004|006|007|009|011")) | !str_detect(name, "C15002"))
#aka if it's an education variable filter it for the total, high school grad, and college grad rows
write_csv(vars_tbl, "data/raw/contextual/variable descriptions.csv")

vars_tbl <- rio::import("data/raw/contextual/formatted vars.xlsx")
vars <- vars_tbl %>% 
  pull(name)
context_data <- get_acs(geography = "tract",variables =  vars, 
                      year = 2018,
                      county = "New Haven", state = "CT")  %>% 
  left_join(vars_tbl, by = c( "variable" = "name")) %>% 
  select(-moe, -NAME, -label, -concept)

#for educational attainment, two vars of prop. high school diploma holders and college graduates (bachelor's + )
#for mobility, prop. moved
#for rent, prop. renting
#for health insurance, prop. without health insurance
races <- c("B","N", "A", "P","O",
           "M", "W", "H")
mobi <- map_dfr(races, function(race){
  sub <- context_data %>% 
    filter(race == !!race, str_detect(variable, "B07004")) %>%
    pivot_wider(names_from = variable, values_from = estimate)
  tot <- str_subset(names(sub), "001")
  prop <- str_subset(names(sub), "002")
  sub <- sub %>% mutate(mobi := 1 - !!sym(prop)/!!sym(tot)) %>% 
      select(GEOID, race, mobi)
  return(sub)
})

rent <- map_dfr(races, function(race){
  sub <- context_data %>% 
    filter(race == !!race, str_detect(variable, "B25003")) %>%
    pivot_wider(names_from = variable, values_from = estimate)
  tot <- str_subset(names(sub), "_001")
  prop <- str_subset(names(sub), "_003")
  sub <- sub %>% mutate(rent := !!sym(prop)/!!sym(tot)) %>% 
    select(GEOID, race, rent)
})

#tiny bit more complicated since there aren't race-specific educational attainment groups, only race-sex-specific ones. Hence:
edu <- map_dfr(races, function(race){
  sub <- context_data %>% 
    filter(race == !!race, str_detect(variable, "C15002")) %>%
    pivot_wider(names_from = variable, values_from = estimate)
  
  totm <- str_subset(names(sub), "_002")
  hsm <- str_subset(names(sub), "_004")
  cm <- str_subset(names(sub), "_006")
  totf <- str_subset(names(sub), "_007")
  hsf <- str_subset(names(sub), "_009")
  cf <- str_subset(names(sub), "_011")
  #overall hs = hsm +hsf / totm + totf
  sub <- sub %>% 
    mutate(hs :=  (!!sym(hsm) + !!sym(hsf))/(!!sym(totm) + !!sym(totf)),
           college :=  (!!sym(cm) + !!sym(cf))/(!!sym(totm) + !!sym(totf))) %>% 
    select(GEOID, race, hs, college)
})

#health insurance is also a pain in the butt because of age categories

ins <- map_dfr(races, function(race){
  sub <- context_data %>% 
    filter(race == !!race, str_detect(variable, "C27001")) %>%
    pivot_wider(names_from = variable, values_from = estimate)
  
  toty <- str_subset(names(sub), "_002")
  ny <- str_subset(names(sub), "_004")
  totm <- str_subset(names(sub), "_005")
  nm <- str_subset(names(sub), "_007")
  toto <- str_subset(names(sub), "_008")
  no <- str_subset(names(sub), "_010")
  #overall hs = hsm +hsf / totm + totf
  sub <- sub %>% 
    mutate(ins = (!!sym(ny) + !!sym(nm) + !!sym(no))/(!!sym(toty)+ !!sym(totm) + !!sym(toto))) %>% 
    select(GEOID, race, ins)
})
inc <- context_data %>% filter(race != "T", str_detect(variable, "B19113"))

full_df <- list(ins, inc, )