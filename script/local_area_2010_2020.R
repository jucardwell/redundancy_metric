##This script uses the nhgis crosswalk table to convert safegraph bg neighborhood analysis from 2010 cbgs to 2020 cbgs
##to match already produced routing. It then joins to baseline routing scenario to serve as input for analysis 
###load libraries
library(tidyverse)
library(sf)
library(tmap)
library(tigris)

##cbg data
cbgs <- st_read("")


#neighborhood analysis for home cbgs
home_cbg <- read_csv("../data/safegraph/fullsafegraph.csv") %>% mutate(GEOID = as.character(GEOID), visitor_home_cbgs = as.character(visitor_home_cbgs))

length(unique(home_cbg$visitor_home_cbgs))
#crosswalk table
crosswalk <- read_csv("") %>% mutate(bg2010ge = as.character(bg2010ge), bg2020ge = as.character(bg2020ge)) %>% select(bg2010ge, bg2020ge, wt_pop)


################################################################
#######TRIPS MOST MONTHS########################################
###11/15/2023#### filter to just trips that are taken at least 50 times 
home_cbg_everymonth <- home_cbg %>% filter(month_count > 50)

##get number of neighbors per cbg
summarize_everymonth <- home_cbg_everymonth %>% group_by(visitor_home_cbgs) %>% summarise(n = n())

#join to crosswalk table to get 2020 bgs
safegraph_change_join_everymonth <- inner_join(home_cbg_everymonth, crosswalk, by = c("visitor_home_cbgs" = "bg2010ge"), relationship = "many-to-many") %>% mutate(updated_count = count * wt_pop) %>% filter(updated_count > 0)

#add together any duplicates that are created during crosswalk table conversion
safegraph_change_aggregated_everymonth <- safegraph_change_join_everymonth %>% group_by(bg2020ge, GEOID) %>% summarise(updated_count = sum(updated_count), month_count = mean(month_count)) 

#filter to just NC, remove intra bg travel
safegraph_filtered_everymonth <- safegraph_change_aggregated_everymonth %>% filter((bg2020ge %in% cbgs$BGGEOID20) & (GEOID %in% cbgs$BGGEOID20) & (GEOID != bg2020ge)) 

#get and display number of neighbors
neighbor_num <- safegraph_filtered_everymonth %>% group_by(bg2020ge) %>% summarise(n = n())
hist(neighbor_num$n)

everymonth_final <- safegraph_filtered_everymonth %>% arrange(desc(updated_count), .by_group = TRUE) %>% mutate(cumsum = cumsum(updated_count), perc = updated_count / sum(updated_count) * 100, cumperc = (cumsum/ sum(updated_count)) * 100)


##add routing information
routable_bg <- read_csv("../data/bg_simp_routable_manual.csv")

#add cbg fid from cbg shapefile
fid_df1 <- merge(everymonth_final, cbgs, by.x = "bg2020ge", by.y = "BGGEOID20", all.x = TRUE) %>% rename(o_fid = fid) %>% select(-POP20, -geometry) 
fid_df2 <- merge(fid_df1, cbgs, by.x = "GEOID", by.y = "BGGEOID20", all.x = TRUE) %>% rename(d_fid = fid) %>% select(-POP20, -geometry)

#add node_id from routing information
routable_df1 <- merge(fid_df2, routable_bg, by.x = "o_fid", by.y =  "fid") %>% rename(o_node_id = node_id)
routable_df2 <- merge(routable_df1, routable_bg, by.x = "d_fid", by.y = "fid") %>% rename(d_node_id = node_id)

#Remove the 3 disconnected bgs. Ocracoke (3259), Bald Head Island (333), north of Corolla (only access is through VA) (1543)
connected_df <- routable_df2 %>% filter(!o_fid %in% c(333,3259,1543) & !d_fid %in% c(333, 3259, 1543)) %>% select(d_fid, o_fid, perc, perc, o_node_id, d_node_id)


write_csv(connected_df, "../data/local_travel_od.csv")

