## This code creates a weighted street network using the dodgr package

library(sf)
library(tidyverse)
library(dodgr)
library(osmdata)
library(sf)

#read in osm data. Replace "" with correct osm network 
nc_roads <- st_read("../data/nc_road_network.shp") 

#remove service roads (driveways, business access, etc.)
nc_roads <- nc_roads %>% filter(highway != "service")

#weight road network
weighted_net <- weight_streetnet(nc_roads, wt_profile = "motorcar", keep_cols = c("name", "oneway"))

#replace "" with streetnet name
dodgr_save_streetnet(weighted_net, "../data/nc_weighted_network.Rds")
