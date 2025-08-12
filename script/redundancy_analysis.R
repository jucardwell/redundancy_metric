library(tidyverse)
library(sf)
library(tmap)
library(broom)
library(tigris)
library(spdep)
library(magick)
library(kableExtra)

file_list <- list.files("output_highway", pattern = "\\.csv$", full.names = TRUE)

# all csvs into list of df, add column 
data_list <- file_list %>% 
  map(~ read_csv(.x)) %>% 
  map2(file_list, ~ mutate(.x,
                           o_fid = sub("matrix.*", "", basename(.y)), 
                           weight = as.numeric(sub(".*weight_([0-9\\.]+)\\.csv", "\\1", basename(.y)))
  ))

# combine
combined_data <- data_list %>% 
  bind_rows() 


combined_data <- combined_data %>%
  pivot_wider(
    names_from = highway, 
    values_from = sum_time_weighted_highway, 
    values_fill = 0 # Fill missing values with 0
  ) %>% rowwise() %>% mutate(total_sum = sum(c_across(motorway:tertiary_link)))

write_csv(combined_data, "combined_redundancy_data_highway_1003_wide.csv")

combined_data <- read_csv("combined_redundancy_data_highway_1003_wide.csv")

cbgs <- st_read("../../../DISS_DATA_SP2023/census_data/nc_blockgroup_centroids_2264.shp") %>% rename(GEOID = BGGEOID20 ) %>% st_drop_geometry()
cbgs <- block_groups(state = "NC", year = 2020, cb = TRUE) %>% left_join(cbgs)

# Compute elbow point per weight and o_fid
total_redundancy <- combined_data %>%
  group_by(weight, o_fid) %>%
  mutate(rat_lower_order = (secondary + secondary_link + tertiary + tertiary_link + unclassified + residential) / total_sum,
    start_point = list(c(min(iteration), min(total_sum))),
    end_point = list(c(max(iteration), max(total_sum))),
    distance = abs((end_point[[1]][2] - start_point[[1]][2]) * iteration - 
                     (end_point[[1]][1] - start_point[[1]][1]) * total_sum) / 
      sqrt((end_point[[1]][2] - start_point[[1]][2])^2 + (end_point[[1]][1] - start_point[[1]][1])^2)
  ) %>%
  ungroup()

add_iteration <- total_redundancy %>%
  group_by(weight, o_fid) %>%
  mutate(elbow = iteration[which.max(distance)])

filtered <- add_iteration %>% group_by(weight, o_fid) %>% filter(iteration <= elbow)

summary <- filtered %>% group_by(weight, o_fid) %>% summarise(iteration = first(elbow), mean_use = mean(rat_lower_order) )

summary_table <- summary %>%
  group_by(weight) %>%
  summarise(
    iteration_min = min(iteration),
    iteration_max = max(iteration),
    iteration_mean = mean(iteration),
    iteration_median = median(iteration),
    iteration_sd = sd(iteration),
    mean_use_mean = mean(mean_use),
    mean_use_sd = sd(mean_use),
    mean_use_median = median(mean_use),
    mean_use_min = min(mean_use),
    mean_use_max = max(mean_use),
    number_tracts = n()
  )

# Display the summary statistics
print(summary_table)

cbg_join <- cbgs %>% left_join(summary, join_by(fid == o_fid)) %>% drop_na()

state <- states(cb = TRUE) %>% filter(NAME == "North Carolina")

ggplot() + 
  geom_sf(data = cbg_join, aes(fill = mean_use), color = NA) +  
  geom_sf(data = state, fill = NA, color = "black") + 
  labs(fill = "Ratio of Lower Roads") +  
  theme_minimal() + 
  facet_wrap(~ weight) + 
  scale_fill_viridis_c() +  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_text(size = 15),  # Increase legend title text size
    legend.text = element_text(size = 15),   # Increase legend labels text size
    strip.text = element_text(size = 15)  
  )

ggplot() + 
  geom_sf(data = cbg_join, aes(fill = iteration), color = NA) +  
  geom_sf(data = state, fill = NA, color = "black") + 
  labs(fill = "Max Iteration") +  
  theme_minimal() + 
  facet_wrap(~ weight) + 
  scale_fill_viridis_c() +  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_text(size = 15),  # Increase legend title text size
    legend.text = element_text(size = 15),   # Increase legend labels text size
    strip.text = element_text(size = 15) 
  )
