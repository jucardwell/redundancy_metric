#this script creates an sf version of the dodgr network

#load libraries
library(dodgr)
library(sf)

#read in osm data
nc_roads <- dodgr_load_streetnet("../data/nc_weighted_network.Rds")

#to sf object
sf_obj <- dodgr_to_sf(nc_roads)

st_write(sf_obj, "../data/nc_sf_network.shp")
