#load libraries
library(tidyverse)
library(sf)
library(tigris)
library(data.table)
library(patchwork)

#get all cbg files in output folder
files <- list.files("output_highway", pattern = "\\.csv$", full.names = TRUE)

#chunk and read in using data.table
chunks <- vector("list", ceiling(length(files) / 500))
k <- 0
for (i in seq(1, length(files), by = 500)) {
  batch <- files[i:min(i + 500 - 1, length(files))]
  batch_list <- lapply(batch, function(f) {
    dt <- fread(f)
    dt[, filename := basename(f)]
    dt
  })
  k <- k + 1
  chunks[[k]] <- rbindlist(batch_list, use.names = TRUE, fill = TRUE)
}

DT <- rbindlist(chunks, use.names = TRUE, fill = TRUE)

#put file name as columns 
DT[, o_fid  := sub("matrix.*", "", filename)]
DT[, weight := as.numeric(sub(".*weight_([0-9\\.]+)\\.csv", "\\1", filename))]

#id columns
id_cols <- setdiff(names(DT), c("highway", "sum_time_weighted_highway"))

#total sum per origin 
totals <- DT[, .(total_sum = sum(sum_time_weighted_highway, na.rm = TRUE)), by = id_cols]

#get elbow point for each weight and o_fid
filtered <- totals %>%
  group_by(weight, o_fid) %>%
  arrange(iteration, .by_group = TRUE) %>%            
  mutate(
    y1 = first(total_sum),                              
    y2 = last(total_sum),                              
    denom = sqrt((y2 - y1)^2 + 14^2),                   
    distance = abs((y2 - y1) * iteration
                   - 14 * total_sum
                   + (15 * y1 - y2)) / denom,
    elbow = iteration[which.max(distance)]
  ) %>%
  filter(iteration <= elbow) %>%
  ungroup()


#summarize to get mean redundancy at elbow iteration
summary_tbl <- filtered %>%
  group_by(weight, o_fid) %>%
  summarise(
    iteration = first(elbow),
    .groups = "drop"
  )

###ADD IN RURALITY AND REGIONS####

#cbg 
cbgs <- st_read("") %>% mutate(county = substr(BGGEOID20, 1, 5)) %>% st_drop_geometry() 

##blocks
nc_blocks <- blocks(state = "NC", year = 2020) %>% mutate(BGGEOID20 = substr(GEOID20, 1, 12)) %>% st_drop_geometry() 

#rural
block_agg <- nc_blocks %>% group_by(BGGEOID20) %>% summarise(total_pop = sum(POP20), rural_pop =  sum(POP20[UR20 == "R"]), perc_rural = rural_pop/total_pop * 100 ) %>% mutate(rural_bg = ifelse(perc_rural > 50, 1, 0)) %>% select(BGGEOID20, rural_bg, perc_rural)

region <- read_csv("north-carolina-geographic-regions.csv") %>%
  mutate(
    county = as.character(fips),
    region = recode(`Region Name`,
                    "Eastern North Carolina" = "Eastern",
                    "Western North Carolina" = "Western",
                    "Central North Carolina" = "Central")
  )
cbgs_with_rural_region <- cbgs %>% left_join(block_agg) %>% left_join(region) %>% mutate(o_fid = as.character(fid)) %>% select(o_fid, rural_bg, region, BGGEOID20, POP20)


summary_with_all <- summary_tbl %>% left_join(cbgs_with_rural_region)

#statewide summary (table 1) 
state_summary <- summary_with_all %>%
  group_by(weight) %>%
  summarise(
    mean_state   = mean(iteration, na.rm = TRUE),
    median_state = median(iteration, na.rm = TRUE),
    sd_state     = sd(iteration, na.rm = TRUE),
    min = min(iteration, na.rm = TRUE),
    max = max(iteration, na.rm = TRUE),
    .groups = "drop"
  )

#regional summary (fig 4)
state_summary <- summary_with_all %>%
  group_by(weight, region) %>%
  summarise(
    mean_state   = mean(iteration, na.rm = TRUE),
    median_state = median(iteration, na.rm = TRUE),
    sd_state     = sd(iteration, na.rm = TRUE),
    min = min(iteration, na.rm = TRUE),
    max = max(iteration, na.rm = TRUE),
    .groups = "drop"
  )
#regional summary compared to mean of each weight
state_scaled <- state_summary %>%
  group_by(weight) %>%
  mutate(
    mean_centered = mean_state - mean(mean_state, na.rm = TRUE)
  ) %>%
  ungroup()

#fig4
p <- ggplot(state_scaled, aes(
  x = region,
  y = factor(weight),
  fill = mean_centered
)) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = paste0(
      "Mean: ", round(mean_state, 2), "\n",
      "SD: ", round(sd_state, 2)
    )),
    size = 3
  ) +
  scale_fill_gradient2(
    low = "#d8b365",
    mid = "white",
    high = "#5ab4ac",
    midpoint = 0,
    name = "Deviation from\npenalty mean"
  ) +
  labs(
    x = "Region",
    y = "Penalty weight",
    title = "Regional deviation from mean redundancy (per penalty weight)",
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "Figure_RegionalDeviation_Heatmap.png",
  plot = p,
  width = 8,
  height = 5, 
  dpi = 600
)


#polygons
cbgs_poly <- block_groups(state = "NC", year = 2020, cb = TRUE) %>%
  left_join(cbgs_with_rural_region, join_by("GEOID" == "BGGEOID20"))

#join to summary data
cbg_join <- cbgs_poly  %>%
  left_join(summary_tbl) %>% drop_na()


###MAKE FIGURE 5
#fips for exmaple counties
urban_ids <- c("119","183","067")   
mixed_ids <- c("013","127","153")
rural_ids <- c("005","121","173")
county_ids_3x3 <- c(urban_ids, mixed_ids, rural_ids)

#
county_lookup <- counties("NC", year = 2020, cb = TRUE) %>%
  st_drop_geometry() %>%
  transmute(COUNTYFP, county_name = NAME)

# function to make map
make_county_map <- function(dat, countyfp, show_legend = FALSE, lookup = county_lookup) {
  df <- dat %>% filter(COUNTYFP == countyfp, weight == 2.00)
  if (!is.numeric(df$iteration)) df <- df %>% mutate(iteration = as.numeric(iteration))
  cname <- lookup$county_name[match(countyfp, lookup$COUNTYFP)]
  if (is.na(cname)) cname <- paste("County", countyfp)
  
  ggplot(df) +
    geom_sf(aes(fill = iteration), color = NA) +
    scale_fill_viridis_c(
      name   = "Redundancy",
      limits = c(1, 9),        # show full 1..9 even if some values are absent
      breaks = 1:9,
      guide  = if (show_legend) guide_colorbar(nbin = 9) else "none"
    ) +
    labs(title = cname) +
    theme_minimal() +
    theme(
      panel.grid   = element_blank(),
      axis.text    = element_blank(),
      axis.ticks   = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title   = element_text(size = 11, face = "bold", hjust = 0.5),
      legend.position = if (show_legend) "right" else "none"
    )
}

# BUILD FIGURE 5
plots_3x3 <- lapply(county_ids_3x3, function(fips) {
  make_county_map(cbg_join, fips, show_legend = (fips == "119"))
})

final_grid <- wrap_plots(plots_3x3, ncol = 3, byrow = TRUE) +
  plot_layout(guides = "collect", heights = rep(1, 3)) &
  theme(legend.position = "right")

#save fig 5
ggsave("grid.png", final_grid,
       width = 8, height = 8, dpi = 600)


# FIGURE 4
nc_counties <- counties(state = "NC", year = 2020, cb = TRUE, class = "sf") %>%
  mutate(county = as.character(GEOID)) %>%
  left_join(region, by = "county")
#get all counties
eastern_counties  <- nc_counties %>% filter(region == "Eastern")
central_counties  <- nc_counties %>% filter(region == "Central")
western_counties  <- nc_counties %>% filter(region == "Western")
#get all cbgs
df_eastern <- cbg_join %>% filter(region == "Eastern",  weight == 2)
df_central <- cbg_join %>% filter(region == "Central",  weight == 2)
df_western <- cbg_join %>% filter(region == "Western",  weight == 2)

#function to make plot for eahc
make_region_plot <- function(df, counties, title) {
  ggplot() +
    geom_sf(data = df, aes(fill = iteration), color = NA) +
    geom_sf(data = counties, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_viridis_c(
      limits = c(1, 9),
      breaks = 1:9,
      name = "Redundancy"
    ) +
    ggtitle(title) +
    theme_void(base_size = 13) +
    theme(
      legend.position = "none",   
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}
 #make plots     
p_east    <- make_region_plot(df_eastern,  eastern_counties,  "Eastern NC")
p_central <- make_region_plot(df_central, central_counties, "Central NC")
p_west    <- make_region_plot(df_western,  western_counties,  "Western NC")

#add plots together
final_plot <- (p_east / p_central / p_west) +
  plot_layout(guides = "collect", heights = c(1, 1, 1)) &
  theme(legend.position = "bottom")

#save fig 3
ggsave(
  "NC_regions_stacked.png",
  final_plot,
  width = 7,
  height = 9,     # not 12
  dpi = 600,
  bg = "white"
)



