#presentation 4-14-20
library(tidyverse)
library(vroom)
library(tmaptools)
library(furrr)
library(sf)
library(ggthemes)
library(viridis)
old_df <- read_csv("data/clean/property_info.csv") %>% 
  mutate(address = paste0(address, ", New Haven, CT,")) 

df <- tibble(match_id = NA, address = NA,  match= NA, 
             match_type = NA, new_address = NA, coords = NA)
df <- map_dfr(list.files("data/clean/geocoded/", full.names=T), function(fn){
  dt <- vroom(fn) 
  dt <- rbind(names(dt), dt)[2:6]
  names(dt) <- c("address", "match","match_type","new_address", "coords")
  return(dt)
})
df <- select(df, address, coords) %>% mutate(address = str_trim(address)) 

df <- left_join(old_df, df, by =  "address") %>% drop_na(coords) 

df <- df %>% 
  separate(coords, into = c("x","y"), sep = ",") %>% 
  st_as_sf(coords = c("x","y"))

nhv <- readRDS("data/clean/nhv_shapefile.RDS") %>% 
  select(GEOID)

st_crs(df) <- st_crs(nhv)

census <- read_csv("data/clean/contextual variables.csv") %>% 
  left_join(nhv, by = "GEOID") %>% 
  filter(race %in%c("B","W")) %>% 
  mutate(race = case_when(race == "B"~ "Black", 
                          race == "W" ~ "White")) %>% 
  drop_na(hs) %>% 
  st_as_sf()

xlabs <- c("High School Diploma Holders", "College Diploma Holders", 
           "Poverty Rate", "Proportion without Health Insurance", 
           "Proportion Moved Within Past 3 Years",
           "Proportion Renting Current Residence", "Proportion of Total Population")

varis <- c("hs", "college","pov","ins", "mobi","rent", "race_prop")
map(varis, function(var){
 var_plot <- ggplot(census, aes(fill = !!sym(var))) + 
    geom_sf(color = NA) + 
    theme_map() + 
    scale_fill_viridis() + 
    facet_wrap(~race) + 
    labs(title = xlabs[which(var == varis)], fill = xlabs[which(var == varis)]) + 
    theme(legend.position = c(.4,-.2), legend.direction = "horizontal", 
         plot.title = element_text(hjust = .5), 
         legend.key.width = unit(1.2, "cm"),
         text = element_text(family = "Segoe UI", size = 16), 
         title = element_text(face = "bold", size = 16), 
         legend.title = element_blank()) + 
    guides(fill = guide_colorbar(title.position = "top", title.hjust = .5))
  
  
  ggsave(paste0("plots/", var, ".png"), var_plot, width = 12, height = 7, dpi = 600)
})


agg <- st_join(nhv, left = F, df) %>% 
  mutate(value = improvement_value + land_value,
         area = living_area + gross_area,
         ratio = value/area) %>% 
  group_by(GEOID) %>%  
  summarise(value = mean(ratio, na.rm=T))


basic_map <- agg %>% 
  filter(value < 600) %>% 
  ggplot(aes(fill=value)) + geom_sf() + 
  theme_map() + 
  scale_fill_viridis()

ggsave("basic map.png", basic_map, width = 8, height = 12, dpi = 600)





ggpairs_census <- census %>% 
  rename(
    "HS Diplomas" = hs, 
    "Poverty Rate" = pov, 
    "College Diplomas"= college, 
    "Without Health Ins." = ins, 
    "Moved Last 3 Years" = mobi, 
    "Prop. of Tract" = race_prop,
    "Prop. Renting" = rent
  )

ggpairs_census <- ggpairs_census %>% select(-GEOID, -estimate) %>% st_drop_geometry %>% drop_na()
black <- ggpairs_census %>% filter(race == "Black")  %>% drop_na %>% select(-race) 
white <- ggpairs_census %>% filter(race == "White")  %>% drop_na %>% select(-race) 

custom_ggpairs <- function(df, race){
  df %>% 
    ggpairs( lower = list(continuous = wrap("points", alpha = 0.3,    size=.2))) + 
    labs(title = paste0("Matrix Plot for ", race, " NHV Residents"))
}

ggsave("Black ggpairs.png", custom_ggpairs(black, "Black"), dpi = 600)
ggsave("White ggpairs.png", custom_ggpairs(white, "White"), dpi = 600)


