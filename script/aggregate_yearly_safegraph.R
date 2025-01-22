## This script aggregates yearly safegraph data into a full dataset and gets just NC data

#load libraries
library(tidyverse)
library(sf)
library(tigris)

#get cbg data
cbgs2010 <- block_groups(state = "NC", year = 2010) 
cbgs2020 <- block_groups(state = "NC", year = 2020)

#get daytime home files
yearly_files <- list.files("../data/safegraph", full.names = TRUE)

##create large dataframe of all years
# Use lapply to read all files and store them in a list
data_list <- lapply(yearly_files, function(file) {
  read.csv(file)  
})

# Use do.call to rbind all the data frames in the list together
combined_data <- do.call(rbind, data_list)

#turn GEOID10 back into character, group by origin and destination and sum counts over all the years. Filter to all NC travel (some originate outside NC or end outside NC)
#Note that home_cbgs need to checked against cbgs2010 and GEOID (or destination geoids) need to be checked by 2020 because 2020 data was applied to POI data in create_yearly_safegraph_OD.R
aggregated_data <- combined_data %>% mutate(GEOID = as.character(GEOID)) %>% group_by(visitor_home_cbgs, GEOID) %>% summarise(count = sum(count), month_count = sum(n_months)) %>% filter((visitor_home_cbgs %in% cbgs2010$GEOID10) & (GEOID %in% cbgs2020$GEOID)) 


write_csv(aggregated_data, "../data/safegraph/full_safegraph.csv")
