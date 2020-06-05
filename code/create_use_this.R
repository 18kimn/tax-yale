library(tidyverse)
library(sf)

full_df <- readRDS("../data/raw/property/full_df.RDS")
rawproperties <- read_sf(dsn="../data/arcgis_output/nhv_addresses_GeocodeAddress.shp")
use_this <- rawproperties %>% bind_cols(full_df)
saveRDS(use_this, "../data/clean/use_this.RDS")