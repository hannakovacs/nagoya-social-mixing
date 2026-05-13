# composite scores & spatial smoothing

library(dplyr)
library(sf)
library(ggplot2)
library(terra)
library(osmdata)
library(data.table)
library(tidyverse)

#setwd(...)

# global setup & functions
print("Loading base grids and municipality map...")
grid_data <- readRDS("04_grid_transit_RAW.rds")
grid_spatial <- readRDS("02_grid_spatial.rds")
grid_income <- readRDS("02_grid_with_income.rds") %>% distinct(x, y, .keep_all = TRUE)
municipality_merged <- readRDS("02_municipality_map_with_income.rds")
grid_mapping <- fread("02_nagoya_grid_coordinates.csv")
municipality_merged_metric <- st_transform(municipality_merged, crs = 2449)

# universal min-max normalization function
normalize_minmax <- function(x) {
  if(all(x == 0) || max(x, na.rm=TRUE) == min(x, na.rm=TRUE)) return(rep(0, length(x)))
  return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
}

# universal 5x5 gaussian kernel matrix
print("Building manual Gaussian matrix (5x5)...")
gaussian_weights <- c(
  1,  4,  6,  4, 1,
  4, 16, 24, 16, 4,
  6, 24, 36, 24, 6,
  4, 16, 24, 16, 4,
  1,  4,  6,  4, 1
)
gaussian_matrix <- matrix(gaussian_weights, nrow = 5, ncol = 5)
gaussian_matrix <- gaussian_matrix / sum(gaussian_matrix)

# merge income data to the base grid right at the start
grid_master <- grid_data %>%
  left_join(
    grid_income %>% st_drop_geometry() %>% select(x, y, rent, income_group), 
    by = c("x", "y")
  )


# 1. velocity score
# calculate raw score
w_rail <- 0.80  
w_bus  <- 0.20  

grid_master <- grid_master %>%
  mutate(Velocity_Raw = (w_rail * log1p(Total_Rail_Stations)) + (w_bus * log1p(Total_Bus_Stops))) %>%
  mutate(Velocity_Score = normalize_minmax(Velocity_Raw)) %>%
  select(-Velocity_Raw)

# plot raw velocity (perfect cells)
plot_data_vel_metric <- grid_master %>% 
  filter(Velocity_Score > 0) %>% 
  st_centroid() %>% 
  st_transform(crs = 2449)

vel_raw_df <- plot_data_vel_metric %>% st_drop_geometry() %>%
  mutate(X_metric = st_coordinates(plot_data_vel_metric)[, 1], Y_metric = st_coordinates(plot_data_vel_metric)[, 2])

p_vel_raw <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = vel_raw_df, aes(x = X_metric, y = Y_metric, fill = Velocity_Score), width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Raw Score\n(0-1)") +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Urban Mobility: Raw Velocity Score", subtitle = "Logarithmic Weighted Density")
ggsave("05.1_velocity_01_raw_map.png", p_vel_raw, width = 10, height = 8, dpi = 300, bg = "white")

# gaussian smoothing for velocity
raster_vel <- grid_master %>% st_drop_geometry() %>% select(x, y, Velocity_Score) %>% 
  as.data.frame() %>% rast(type = "xyz", crs = "EPSG:4326")

smoothed_vel <- focal(raster_vel, w = gaussian_matrix, fun = "sum", na.rm = TRUE, pad = TRUE, padValue = 0)
smoothed_df_vel <- as.data.frame(smoothed_vel, xy = TRUE)
names(smoothed_df_vel)[3] <- "Velocity_Smoothed_Temp" 

grid_master <- grid_master %>%
  left_join(smoothed_df_vel, by = c("x", "y")) %>%
  mutate(Velocity_Smoothed_Temp = ifelse(is.na(Velocity_Smoothed_Temp), 0, Velocity_Smoothed_Temp)) %>%
  mutate(Velocity_Score_Smoothed = normalize_minmax(Velocity_Smoothed_Temp)) %>%
  select(-Velocity_Smoothed_Temp)

# plot smoothed velocity (perfect cells)
plot_data_vel_sm_metric <- grid_master %>% 
  filter(Velocity_Score_Smoothed > 0) %>% 
  st_centroid() %>% 
  st_transform(crs = 2449)

vel_sm_df <- plot_data_vel_sm_metric %>% st_drop_geometry() %>%
  mutate(X_metric = st_coordinates(plot_data_vel_sm_metric)[, 1], Y_metric = st_coordinates(plot_data_vel_sm_metric)[, 2])

p_vel_sm <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = vel_sm_df, aes(x = X_metric, y = Y_metric, fill = Velocity_Score_Smoothed), width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Normalized Score\n(0-1)") +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.background = element_rect(fill = "white", color = NA))
ggsave("05.1_velocity_02_smoothed_map.png", p_vel_sm, width = 6, height = 4, dpi = 300, bg = "white")



# 2. livability score

# calculate raw score
set_overpass_url("https://overpass-api.de/api/interpreter")
mobility_extent <- st_bbox(grid_spatial)

extract_osm_points <- function(osm_data, category_name) {
  points <- st_sf(geometry = st_sfc(crs = 4326))
  if (!is.null(osm_data$osm_points) && nrow(osm_data$osm_points) > 0) {
    p <- osm_data$osm_points %>% select(geometry)
    points <- bind_rows(points, p)
  }
  if (!is.null(osm_data$osm_polygons) && nrow(osm_data$osm_polygons) > 0) {
    poly_centroids <- st_centroid(osm_data$osm_polygons$geometry)
    poly_points <- st_sf(geometry = poly_centroids)
    points <- bind_rows(points, poly_points)
  }
  if (nrow(points) > 0) points$livability_category <- category_name
  return(points)
}

# fetch osm data (education, healthcare, groceries)
q_edu <- opq(bbox = mobility_extent, timeout = 300) %>% add_osm_feature(key = 'amenity', value = c('school', 'kindergarten'))
edu_points <- extract_osm_points(osmdata_sf(q_edu), "Education")

q_health <- opq(bbox = mobility_extent, timeout = 300) %>% add_osm_feature(key = 'amenity', value = c('hospital', 'clinic', 'pharmacy', 'doctors'))
health_points <- extract_osm_points(osmdata_sf(q_health), "Healthcare")

q_groceries <- opq(bbox = mobility_extent, timeout = 300) %>% add_osm_feature(key = 'shop', value = c('supermarket', 'convenience'))
grocery_points <- extract_osm_points(osmdata_sf(q_groceries), "Groceries")

all_livability_sf <- bind_rows(edu_points, health_points, grocery_points)
livability_in_grid <- st_join(all_livability_sf, grid_spatial, join = st_nearest_feature)
setDT(livability_in_grid)

livability_summary <- livability_in_grid[, .(Livability_Count = .N, Livability_Diversity = uniqueN(livability_category)), by = .(x, y)]


# plot livability categories with grid cells

library(patchwork)
library(tidyr)

# reshape the data to wide format (counting points per category per cell)
# using data.table's dcast since livability_in_grid is already a data.table
livability_wide <- dcast(livability_in_grid, x + y ~ livability_category, 
                         value.var = "livability_category", 
                         fun.aggregate = length)

# merge with the spatial grid, replace NAs with 0, and transform to metric (EPSG:2449)
plot_data_liv_metric <- grid_spatial %>%
  left_join(livability_wide, by = c("x", "y")) %>%
  mutate(across(c(Education, Healthcare, Groceries), ~replace_na(., 0))) %>%
  st_transform(crs = 2449) %>%
  st_centroid() # extract centroids for the tiles

# create a standard dataframe with X and Y metric coordinates
liv_df <- plot_data_liv_metric %>%
  st_drop_geometry() %>%
  mutate(
    X_metric = st_coordinates(plot_data_liv_metric)[, 1],
    Y_metric = st_coordinates(plot_data_liv_metric)[, 2]
  )

# plot livability categories (3 side-by-side)

# clean theme: zero margins, custom legend proportions (wider and shorter)
mapping_theme <- theme_minimal() + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    
    # keep only the category names as titles
    plot.title = element_text(size = 15, face = "plain", hjust = 0.5, margin = margin(b = 2)),
    
    # strictly zero margins around the maps
    plot.margin = margin(2, 2, 2, 2, "pt"), 
    panel.spacing = unit(0, "lines"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # legend adjustments: wider (0.5cm) and shorter (0.4cm)
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 7),
    
    # keep the legend pulled tight to the map
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, -10)
  )

# generating the 3 maps

# education 
p_edu <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = liv_df %>% filter(Education > 0), 
            aes(x = X_metric, y = Y_metric, fill = Education), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "magma", end = 0.9, direction = -1, name = "Count") +
  coord_sf(datum = NA, expand = FALSE) + 
  mapping_theme +
  labs(title = "Education")

# healthcare 
p_health <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = liv_df %>% filter(Healthcare > 0), 
            aes(x = X_metric, y = Y_metric, fill = Healthcare), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "magma", end = 0.9, direction = -1, name = "Count") +
  coord_sf(datum = NA, expand = FALSE) + 
  mapping_theme +
  labs(title = "Healthcare")

# groceries 
p_groceries <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = liv_df %>% filter(Groceries > 0), 
            aes(x = X_metric, y = Y_metric, fill = Groceries), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "magma", end = 0.9, direction = -1, name = "Count") +
  coord_sf(datum = NA, expand = FALSE) + 
  mapping_theme +
  labs(title = "Groceries")

# patchworking maps together (simple side-by-side layout, no main title)
final_liv_map <- p_edu | p_health | p_groceries

print(final_liv_map)


grid_master <- merge(grid_master, livability_summary, by = c("x", "y"), all.x = TRUE) %>%
  mutate(
    Livability_Count = ifelse(is.na(Livability_Count), 0, Livability_Count),
    Livability_Diversity = ifelse(is.na(Livability_Diversity), 0, Livability_Diversity),
    Livability_Raw = log1p(Livability_Count) * (Livability_Diversity + 1),
    Livability_Score = normalize_minmax(Livability_Raw)
  ) %>%
  select(-Livability_Raw)

# plot raw livability (perfect cells)
plot_data_liv_raw_metric <- merge(grid_master, grid_mapping[, .(x, y, lon, lat)], by = c("x", "y"), all.x = TRUE) %>%
  filter(!is.na(lon) & Livability_Score > 0) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(crs = 2449)

liv_raw_df <- plot_data_liv_raw_metric %>% st_drop_geometry() %>%
  mutate(X_metric = st_coordinates(plot_data_liv_raw_metric)[, 1], Y_metric = st_coordinates(plot_data_liv_raw_metric)[, 2])

p_liv_raw <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray96", color = "white", linewidth = 0.2) +
  geom_tile(data = liv_raw_df, aes(x = X_metric, y = Y_metric, fill = Livability_Score), width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "A", direction = -1, name = "Raw Score\n(0-1)", limits = c(0, 1)) +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Urban Livability: Raw Index", subtitle = "Schools, Healthcare, Groceries Density-Diversity")
ggsave("05.1_livability_01_raw_map.png", p_liv_raw, width = 10, height = 8, dpi = 300, bg = "white")

# gaussian smoothing for livability
raster_liv <- grid_master %>% st_drop_geometry() %>% select(x, y, Livability_Score) %>% 
  as.data.frame() %>% rast(type = "xyz", crs = "EPSG:4326")

smoothed_liv <- focal(raster_liv, w = gaussian_matrix, fun = "sum", na.rm = TRUE, pad = TRUE, padValue = 0)
smoothed_df_liv <- as.data.frame(smoothed_liv, xy = TRUE)
names(smoothed_df_liv)[3] <- "Livability_Smoothed_Temp" 

grid_master <- grid_master %>%
  left_join(smoothed_df_liv, by = c("x", "y")) %>%
  mutate(Livability_Smoothed_Temp = ifelse(is.na(Livability_Smoothed_Temp), 0, Livability_Smoothed_Temp)) %>%
  mutate(Livability_Score_Smoothed = normalize_minmax(Livability_Smoothed_Temp)) %>%
  select(-Livability_Smoothed_Temp)

# plot smoothed livability (perfect cells)
plot_data_liv_sm_metric <- merge(grid_master, grid_mapping[, .(x, y, lon, lat)], by = c("x", "y"), all.x = TRUE) %>%
  filter(!is.na(lon) & Livability_Score_Smoothed > 0) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(crs = 2449)

liv_sm_df <- plot_data_liv_sm_metric %>% st_drop_geometry() %>%
  mutate(X_metric = st_coordinates(plot_data_liv_sm_metric)[, 1], Y_metric = st_coordinates(plot_data_liv_sm_metric)[, 2])

p_liv_sm <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray96", color = "white", linewidth = 0.2) +
  geom_tile(data = liv_sm_df, aes(x = X_metric, y = Y_metric, fill = Livability_Score_Smoothed), width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "A", direction = -1, name = "Normalized Score\n(0-1)", limits = c(0, 1)) +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.background = element_rect(fill = "white", color = NA))
ggsave("05.1_livability_02_smoothed_map.png", p_liv_sm, width = 6, height = 4, dpi = 300, bg = "white")



# 3. attractivity score
# calculate raw score
grid_with_pois <- readRDS("03_grid_with_pois.rds") %>% st_as_sf()
grid_master <- st_join(st_as_sf(grid_master), grid_with_pois) %>%
  # clean potential .y coordinate columns right away
  select(-any_of(c("x.y", "y.y"))) %>% rename_with(~gsub(".x$", "", .x))

w_culture  <- 1.5  
w_parks    <- 1.2  
w_shopping <- 1.0  
w_food     <- 0.8  

grid_master <- grid_master %>%
  mutate(Attractivity_Raw = (log1p(Culture_Entertainment) * w_culture + log1p(Parks_Sports) * w_parks + 
                               log1p(Shopping) * w_shopping + log1p(Food_Drink) * w_food),
         poi_diversity = ((Culture_Entertainment > 0) + (Parks_Sports > 0) + (Shopping > 0) + (Food_Drink > 0)),
         Attractivity_Score = normalize_minmax(Attractivity_Raw * (poi_diversity + 1))) %>%
  select(-Attractivity_Raw, -poi_diversity)

# plot raw attractivity (perfect cells)
plot_data_attr_raw_metric <- grid_master %>% 
  st_drop_geometry() %>%
  select(-any_of(c("lon", "lat", "lon.x", "lat.x", "lon.y", "lat.y"))) %>% 
  left_join(grid_mapping %>% select(x, y, lon, lat), by = c("x", "y")) %>%
  filter(!is.na(lon) & Attractivity_Score > 0) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(crs = 2449)

attr_raw_df <- plot_data_attr_raw_metric %>% st_drop_geometry() %>%
  mutate(X_metric = st_coordinates(plot_data_attr_raw_metric)[, 1], Y_metric = st_coordinates(plot_data_attr_raw_metric)[, 2])

p_attr_raw <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray96", color = "white", linewidth = 0.2) +
  geom_tile(data = attr_raw_df, aes(x = X_metric, y = Y_metric, fill = Attractivity_Score), width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "inferno", direction = -1, name = "Raw Score\n(0-1)", limits = c(0, 1)) +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Urban Attractivity: Regional Magnets", subtitle = "Raw Density and Complexity of POIs")
ggsave("05.1_attractivity_01_raw_map.png", p_attr_raw, width = 10, height = 8, dpi = 300, bg = "white")

# gaussian smoothing for attractivity
raster_attr <- grid_master %>% st_drop_geometry() %>% select(x, y, Attractivity_Score) %>% 
  as.data.frame() %>% rast(type = "xyz", crs = "EPSG:4326")

smoothed_attr <- focal(raster_attr, w = gaussian_matrix, fun = "sum", na.rm = TRUE, pad = TRUE, padValue = 0)
smoothed_df_attr <- as.data.frame(smoothed_attr, xy = TRUE)
names(smoothed_df_attr)[3] <- "Attractivity_Smoothed_Temp" 

grid_master <- grid_master %>%
  left_join(smoothed_df_attr, by = c("x", "y")) %>%
  mutate(Attractivity_Smoothed_Temp = ifelse(is.na(Attractivity_Smoothed_Temp), 0, Attractivity_Smoothed_Temp)) %>%
  mutate(Attractivity_Score_Smoothed = normalize_minmax(Attractivity_Smoothed_Temp)) %>%
  select(-Attractivity_Smoothed_Temp)

# plot smoothed attractivity (perfect cells)
plot_data_attr_sm_metric <- grid_master %>% 
  st_drop_geometry() %>%
  select(-any_of(c("lon", "lat", "lon.x", "lat.x", "lon.y", "lat.y"))) %>% 
  left_join(grid_mapping %>% select(x, y, lon, lat), by = c("x", "y")) %>%
  filter(!is.na(lon) & Attractivity_Score_Smoothed > 0) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(crs = 2449)

attr_sm_df <- plot_data_attr_sm_metric %>% st_drop_geometry() %>%
  mutate(X_metric = st_coordinates(plot_data_attr_sm_metric)[, 1], Y_metric = st_coordinates(plot_data_attr_sm_metric)[, 2])

p_attr_sm <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray96", color = "white", linewidth = 0.2) +
  geom_tile(data = attr_sm_df, aes(x = X_metric, y = Y_metric, fill = Attractivity_Score_Smoothed), width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  scale_fill_viridis_c(option = "A", direction = -1, name = "Normalized Score\n(0-1)", limits = c(0, 1)) +
  theme_minimal() + theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.background = element_rect(fill = "white", color = NA))
ggsave("05.1_attractivity_02_smoothed_map.png", p_attr_sm, width = 6, height = 4, dpi = 300, bg = "white")


# final save
saveRDS(grid_master, "05.1_grid_MASTER_SMOOTHED.rds")
