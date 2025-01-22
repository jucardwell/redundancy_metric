##load libraries
library(dodgr)
library(tidyverse)
library(sf)
library(reshape2)
library(tigris)

#cbgs
cbgs <- st_read("") %>% mutate(GEOID = substr(BGGEOID20, 1, 5))


##analysis cbgs. Rural counties in Mountains and Coastal Plain (defined by NC Rural Center)
rural_counties <- c("Currituck", "Camden", "Pasquotank", "Perquimans", "Gates", "Chowan", "Hertford", "Northampton", "Bertie",
                    "Martin", "Washington", "Tyrrell", "Hyde", "Dare", "Beaufort", "Greene", "Craven", "Pamlico", "Lenoir", "Jones",
                    "Carteret", "Sampson", "Duplin", "Pender",  "Brunswick", "Halifax", "Nash", "Edgecombe", "Wilson",
                    "Wayne", "Harnett", "Bladen", "Columbus", "Robeson", "Cherokee", "Clay", "Macon", "Jackson", "Transylvania", "Swain", "Graham", "Haywood", "Madison", "Yancey", "Mitchell", "McDowell", "Rutherford", "Polk", "Cleveland", "Burke",
                    "Avery", "Watauga", "Ashe", "Alleghany", "Wilkes", "Caldwell")

#get cbgs in analysis counties
analysis_counties <- counties(state = "NC", year = 2020) %>% filter(NAME %in% rural_counties)

#filter only to analysis
analysis_cbgs <- cbgs %>% filter(GEOID %in% analysis_counties$GEOID)

#get travel matrix
travel_od <- read_csv("../data/safegraph/fullsafegraph.csv") %>% left_join(cbgs, by = c("o_fid" = "fid")) %>%
  mutate(trips = (perc/100) * POP20, o_node_id = as.character(o_node_id), d_node_id = as.character(d_node_id)) 

#get only analysis cbgs
rural_travel_od <- travel_od %>% filter(o_fid %in% analysis_cbgs$fid)
origins <- unique(rural_travel_od$o_fid)

#create travel matrix flow for each analysis census block group
for (origin in origins){
  fid <- origin
  sample <- rural_travel_od %>% filter(o_fid == fid)
  wide_data_scenario <- dcast(sample, o_node_id ~ d_node_id, value.var = "trips", fill = 0, fun.aggregate = sum)
  matrix_data_scenario <- as.matrix(wide_data_scenario[, -1]) # Exclude the first column which contains the row names
  rownames(matrix_data_scenario) <- wide_data_scenario$o_node_id
  saveRDS(matrix_data_scenario, paste0("../data/travel_matrices/", fid, "matrix.rds"))
  
}


