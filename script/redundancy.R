##load libraries
library(dodgr)
library(tidyverse)


# Set seed for reproducibility
set.seed(100)

args <- commandArgs(trailingOnly = TRUE)
file <- args[1]

file_base <- basename(file)

file_path <- paste("../data/travel_matrices/", file, sep = "")

#read in each separate matrix
matrix_data_scenario <- readRDS(file_path)


from <- rownames(matrix_data_scenario)
to <- colnames(matrix_data_scenario)

####Read in Road Data#####
#read in osm data
nc_roads <- dodgr_load_streetnet("../data/nc_weighted_network.Rds")
nc_roads <- dodgr_contract_graph(nc_roads)
#get largest component
largest_component <- nc_roads[nc_roads$component == 1, ]
largest_component$variable_time <- largest_component$time_weighted
largest_component$d_weighted <- largest_component$variable_time
#######################


num_scenarios <- 30
  weight_increase_factor <- c(1.25, 1.5, 1.75, 2)
  
  #iterate through each weight increase factor
  for (j in 1:length(weight_increase_factor)) {
    baseline_road_set <- vector("list", num_scenarios)
    weight_factor <- weight_increase_factor[j]
    largest_component_scenario <- largest_component
    
    penalized_roads <- vector() # Store all penalized roads here
    
    #iterate through each scenario
    for (k in 1:num_scenarios) {
      #weight adjustment after the first iteration
      if (k > 1) {
        used_roads <- baseline_road_set %>%
          bind_rows() %>%
          pull(edge_id) %>%
          unique() %>%
          as.character()
        
        #add the newly used roads to the penalized roads, but ensure no road is penalized more than once
        new_penalized_roads <- setdiff(used_roads, penalized_roads)
        penalized_roads <- unique(c(penalized_roads, new_penalized_roads))
        
        #adjust the weights only for the newly penalized roads
        largest_component_scenario$variable_time <- ifelse(largest_component_scenario$edge_id %in% new_penalized_roads,
                                                           largest_component_scenario$variable_time * weight_factor,
                                                           largest_component_scenario$variable_time)
        largest_component_scenario$d_weighted <- largest_component_scenario$variable_time
      }
      
      #perform dodgr flows aggregate for the scenario
      graph_baseline <- dodgr_flows_aggregate(largest_component_scenario, from = from, to = to, flows = matrix_data_scenario, norm_sums = FALSE, contract = FALSE)
      
      #filter and select data
      filtered_data <- graph_baseline %>%
        as_tibble() %>%
        filter(flow > 0) %>%
        select(edge_id, flow, d_weighted, highway) %>%
        mutate(iteration = k, time_weighted_flow = flow * d_weighted)
      
      #store Used Roads in the baseline_road_set
      baseline_road_set[[k]] <- filtered_data
    }
    
    #combine the roads used across all iterations
    complete_used_roads <- baseline_road_set %>%
      bind_rows()
    
    #summarize the time weighted flow by iteration
    summarized_iterations <- complete_used_roads %>%
      group_by(iteration, highway) %>%
      summarise(sum_time_weighted_highway = sum(time_weighted_flow))
    
    #save the result to a csv file
    write_csv(summarized_iterations, paste0("../output/redundancy_output/", file_base, "_output_weight_", weight_factor, ".csv"))
  }