## This script extracts the OSM network using the osmextract package

#libraries
library(osmdata)
library(osmextract)
library(sf)

#add extra tags for routing
et = c("oneway", "maxspeed", "lanes") 

#extract nc road network using osmextract
nc_roads <- oe_get_network(place = "north carolina", mode = "driving", extra_tags = et)

#add id
nc_roads$seg_id <- seq.int(nrow(nc_roads))

#write. Replace "" with file name
st_write(nc_roads, "../data/nc_road_network.shp")
