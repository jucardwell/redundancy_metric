##load libraries
library(dodgr)
library(tidyverse)

#get argument from .sh file (i.e. each unique od matrix)
args <- commandArgs(trailingOnly = TRUE)
file <- args[1]

file_base <- basename(file)

#read in the travel matrix
file_path <- paste("/work/users/j/m/jmcard/redundancy/travel_matrices_full_state/", file, sep = "")

#set directory for unique (unaggregated) redundancy output (i.e. list of road segment ids)
out_dir_unique  <- ""
#set directory for aggregated redundancy output
out_dir_highway <- ""

matrix_data_scenario <- readRDS(file_path)

#to from notes
from <- rownames(matrix_data_scenario)
to   <- colnames(matrix_data_scenario)

#network graph
largest_component <- readRDS("")

## parameters
num_scenarios <- 15
##penalty factor
weight_increase_factor <- c(1.25, 1.5, 1.75, 2)

##for each penalty weight
for (weight_factor in weight_increase_factor) {
  #set empty vector for baseline road segments
  baseline_road_set <- vector("list", num_scenarios)
  #new instance for largest component
  largest_component_scenario <- largest_component

  #roads used so far
  used_so_far <- character(0)
  #penalized roads
  penalized_roads <- character(0)

  #for each scenario
  for (k in seq_len(num_scenarios)) {
    #if not first scenario
    if (k > 1) {
      #penalize roads
      new_penalize <- setdiff(used_so_far, penalized_roads)
      if (length(new_penalize) > 0) {
        penalized_roads <- c(penalized_roads, new_penalize)
        idx <- largest_component_scenario$edge_id %in% new_penalize
        largest_component_scenario$variable_time[idx] <-
          largest_component_scenario$variable_time[idx] * weight_factor
        largest_component_scenario$d_weighted <- largest_component_scenario$variable_time
      }
    }
    #then calculate flows 
    graph_baseline <- dodgr_flows_aggregate(
      largest_component_scenario,
      from = from, to = to,
      flows = matrix_data_scenario,
      norm_sums = FALSE,
      contract = FALSE
    )
    #just get used roads
    filtered_data <- graph_baseline %>%
      as_tibble() %>%
      filter(flow > 0) %>%
      select(edge_id, flow, d_weighted, highway) %>%
      mutate(iteration = k, time_weighted_flow = flow * d_weighted)

    baseline_road_set[[k]] <- filtered_data
    used_so_far <- unique(c(used_so_far, filtered_data$edge_id))
  }
  #all used boards
  complete_used_roads <- bind_rows(baseline_road_set)

  # unique roads and time used
  iter_edge_counts <- complete_used_roads %>%
    arrange(iteration) %>%
    filter(!duplicated(edge_id)) %>%
    count(iteration, name = "n_edges") %>%
    arrange(iteration)
#write
  write_csv(
    iter_edge_counts,
    file.path(out_dir_unique, paste0(file_base, "_iteredges_weight_", weight_factor, "_counts.csv"))
  )

  # aggregated to type of road
  summarized_iterations <- complete_used_roads %>%
    group_by(iteration, highway) %>%
    summarise(sum_time_weighted_highway = sum(time_weighted_flow), .groups = "drop")

  write_csv(
    summarized_iterations,
    file.path(out_dir_highway, paste0(file_base, "_output_weight_", weight_factor, ".csv"))
  )
}
