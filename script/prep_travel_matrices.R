##load libraries
library(dodgr)
library(tidyverse)
library(sf)
library(reshape2)
library(tigris)

#cbgs
cbgs <- st_read("../../../DISS_DATA_SP2023/census_data/nc_blockgroup_centroids_2264.shp") %>% mutate(GEOID = substr(BGGEOID20, 1, 5))

#blocks to determine rural or urban
nc_blocks <- blocks(state = "NC", year = 2020) %>% mutate(BGGEOID20 = substr(GEOID20, 1, 12)) %>% st_drop_geometry()

block_agg <- nc_blocks %>% group_by(BGGEOID20) %>% summarise(total_pop = sum(POP20), rural_pop =  sum(POP20[UR20 == "R"]), perc_rural = rural_pop/total_pop * 100 ) %>% mutate(rural_bg = ifelse(perc_rural > 50, 1, 0)) %>% select(BGGEOID20, rural_bg)

joined_cbgs <- cbgs %>% left_join(block_agg)

write_csv(joined_cbgs, "rural_bg_08212025.csv")

#fill in with location of travel od
travel_od <- read_csv("") %>% left_join(cbgs, by = c("o_fid" = "fid")) %>%
  mutate(trips = (perc/100) * POP20, o_node_id = as.character(o_node_id), d_node_id = as.character(d_node_id)) 

#create rds object for each origin
for (origin in origins){
  fid <- origin
  sample <- travel_od %>% filter(o_fid == fid)
  wide_data_scenario <- dcast(test, o_node_id ~ d_node_id, value.var = "trips", fill = 0, fun.aggregate = sum)
  matrix_data_scenario <- as.matrix(wide_data_scenario[, -1]) #exclude the first column which contains the row names
  rownames(matrix_data_scenario) <- wide_data_scenario$o_node_id
  saveRDS(matrix_data_scenario, paste0("travel_matrices_full_state/", fid, "matrix.rds"))
  
}

#no safegraph data
missing <- c(2299, 2633, 2987, 3000, 3003, 3097, 3113, 3247, 2622, 3815, 3889, 4046, 4105, 4289, 4455, 4601, 4633, 4692, 4749, 4754, 5072, 5242, 5297, 5469, 5626, 5652, 5726, 5743, 6029, 613, 6154, 6197, 6539, 6562, 6733, 6773, 755, 903)

setDT(travel_od)

for (fid in missing) {
  sample <- travel_od[o_fid == fid]
  
  all_o <- sort(unique(sample$o_node_id))
  all_d <- sort(unique(sample$d_node_id))


  sample[, o_node_id := factor(o_node_id, levels = all_o)]
  sample[, d_node_id := factor(d_node_id, levels = all_d)]
  
  wide_data_scenario <- dcast(
    sample,
    o_node_id ~ d_node_id,
    value.var     = "trips",
    fun.aggregate = sum,
    drop          = FALSE,
    fill          = 0
  )
  
  #convert to matrix without dropping dims when there’s only 1 column
  mat <- as.matrix(wide_data_scenario[, -1, with = FALSE])
  rownames(mat) <- as.character(wide_data_scenario$o_node_id)
  
  saveRDS(mat, paste0("travel_matrices_full_state_fixed/", fid, "matrix.rds"))
}


