##this script creates a yearly safegraph OD from the monthly data

#load libraries
library(tidyverse)
library(sf)
library(tigris)

#years of safegraph data
list_years <- c("2018", "2019", "2020", "2021", "2022")


#get census bg boundaries from tigris. Some of the the poi info files do not have cbg. Updated 11/1 to use
#2020 for getting bg of POIs. This makes it simpler when converting from 2010 - 2020 later in the process.
census_bgs <- block_groups(state = "NC", year = 2020) %>% st_transform(4326)

#get od files
od_files <- list.files("", full.names = TRUE)

#get poi files
poi_files <- list.files("", full.names = TRUE)

#for each year
for (year in list_years){
  print(year)
  #filter to year of interest
  filtered_odfiles <- od_files[substr(od_files, 80, 83) == year]
  filtered_poifiles <- poi_files[substr(poi_files, 82, 85) == year]
  #initialize data frame
  yearly_df <- data.frame()
  #for each file in the year (monthly)
  for (i in 1:length(filtered_odfiles)){
    #read in file and make sure cbgs are character (in some cases they were reading in as numeric)
    od <- read_csv(filtered_odfiles[i]) %>% mutate(visitor_home_cbgs = as.character(visitor_home_cbgs))
    #read in poi data. Not all pois had a GEOID20, so spatial join to identify GEOID20 for each poi based on lat, lon 
    poi <- read_csv(filtered_poifiles[i]) %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_join(census_bgs) %>% select(placekey, GEOID) %>% st_drop_geometry()
    #merge data so that OD poi also has a GEOID20 so that we can match home cbgs to poi cbgs
    od_poi_cbgs <- merge(od, poi, by = "placekey") %>% group_by(visitor_home_cbgs, GEOID) %>% summarise(count = sum(count))
    #bind to yearly dataframe
    yearly_df <- rbind(yearly_df, od_poi_cbgs)
  }
  #aggregate for yearly data
  agg_yearly_df <- yearly_df %>% group_by(visitor_home_cbgs, GEOID) %>% summarise(count = sum(count), n_months = n())
  write_csv(agg_yearly_df, paste("../data/safegraph/", year, "_2010cbg_home.csv", sep = ""))
}




