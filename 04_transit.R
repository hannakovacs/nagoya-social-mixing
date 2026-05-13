# part 1: transit data collection (rail & bus)

library(osmdata)
library(sf)
library(dplyr)
library(tidytransit)

# setwd(...)

# load base spatial grid
print("Loading base spatial grid...")
grid_spatial <- readRDS("02_grid_spatial.rds")
nagoya_bbox  <- st_bbox(grid_spatial)

# fetch rail data from OpenStreetMap
print("Fetching Rail data from OpenStreetMap...")
set_overpass_url("https://overpass-api.de/api/interpreter")

q_rail <- opq(bbox = nagoya_bbox, timeout = 300) %>% 
  add_osm_feature(key = 'railway', value = c('station', 'tram_stop', 'halt'))

rail_osm <- osmdata_sf(q_rail)$osm_points

# deduplicate coordinates
rail_stops_sf <- rail_osm %>%
  select(geometry) %>%
  distinct(geometry, .keep_all = TRUE)

# count rail stations per cell
rail_grid_counts <- st_join(rail_stops_sf, grid_spatial, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  group_by(x, y) %>%
  summarise(Total_Rail_Stations = n(), .groups = "drop")

# process GTFS bus files
print("Processing GTFS Bus files...")
gtfs_folder <- "C:/Egyetem/SDS/THESIS/GTFS_Buses"
zip_files   <- list.files(gtfs_folder, pattern = "\\.zip$", full.names = TRUE)

all_stops_list <- list()

for (file in zip_files) {
  tryCatch({
    gtfs_data <- read_gtfs(file)
    stops_sf  <- stops_as_sf(gtfs_data$stops) %>% st_transform(4326)
    all_stops_list[[basename(file)]] <- stops_sf %>% select(geometry)
  }, error = function(e) { cat("Error in:", basename(file), "\n") })
}

# bind and crop bus stops
all_bus_sf <- do.call(rbind, all_stops_list)
all_bus_cropped <- st_crop(all_bus_sf, nagoya_bbox)

# count bus stops per cell
bus_grid_counts <- st_join(all_bus_cropped, grid_spatial, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  group_by(x, y) %>%
  summarise(Total_Bus_Stops = n(), .groups = "drop")

# merge transit counts into the base grid
print("Merging transit data into base grid...")
grid_transit_raw <- grid_spatial %>%
  left_join(rail_grid_counts, by = c("x", "y")) %>%
  left_join(bus_grid_counts, by = c("x", "y")) %>%
  mutate(
    Total_Rail_Stations = ifelse(is.na(Total_Rail_Stations), 0, Total_Rail_Stations),
    Total_Bus_Stops     = ifelse(is.na(Total_Bus_Stops), 0, Total_Bus_Stops)
  )

# save raw transit counts for part 2
saveRDS(grid_transit_raw, "04_grid_transit_RAW.rds")
print("Data collection complete! Saved as '04_grid_transit_RAW.rds'")


# part 1/b: plotting raw transit networks

library(ggplot2)
library(sf)
library(dplyr)

print("Generating Rail and Bus Network Maps...")

# load background map for Nagoya
# assuming this was created in a previous script
municipality_merged <- readRDS("02_municipality_map_with_income.rds")


# plot 1: rail network map

# filter cells with at least one rail station and calculate centroids
plot_data_rail <- grid_transit_raw %>% 
  filter(Total_Rail_Stations > 0) %>%
  st_centroid()

rail_map <- ggplot() +
  # background municipality map
  geom_sf(data = municipality_merged, fill = "gray95", color = "white", linewidth = 0.3) +
  # rail station points
  geom_sf(data = plot_data_rail, aes(color = Total_Rail_Stations), size = 1.5, alpha = 0.85) +
  # color scale (option B = inferno) - adjusted with 'end' to start darker
  scale_color_viridis_c(
    option = "B", 
    direction = -1, 
    trans = "pseudo_log",
    breaks = c(1, 2, 5, 10, 20), 
    name = "Rail Stations\n(per Cell)",
    end = 0.9
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Rail Transit Accessibility")

print(rail_map)
ggsave("04.3_raw_rail_density_map.png", rail_map, width = 10, height = 8, dpi = 300)


# plot 2: bus network map (with perfect cells)

library(dplyr)
library(sf)
library(ggplot2)

print("Converting data to metric cells for the bus map...")

# filter data, transform to Japanese metric projection (EPSG:2449), and extract centroids
plot_data_bus_metric <- grid_transit_raw %>% 
  filter(Total_Bus_Stops > 0) %>%
  st_transform(crs = 2449) %>%
  st_centroid() # ensure clean X-Y coordinates for the cells

municipality_merged_metric <- st_transform(municipality_merged, crs = 2449)

# extract metric X and Y coordinates into a simple dataframe for geom_tile()
bus_df <- plot_data_bus_metric %>%
  st_drop_geometry() %>%
  mutate(
    X_metric = st_coordinates(plot_data_bus_metric)[, 1],
    Y_metric = st_coordinates(plot_data_bus_metric)[, 2]
  )

# generate map with cells (same logic as entropy)
bus_map <- ggplot() +
  # background: Nagoya outlines
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  
  # foreground: perfect cells instead of points (geom_tile)
  geom_tile(data = bus_df, aes(x = X_metric, y = Y_metric, fill = Total_Bus_Stops), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 0.85) +
  
  # apply color scale for cell fill
  scale_fill_viridis_c(
    option = "B", direction = -1, trans = "pseudo_log",
    breaks = c(1, 3, 10, 30, 100), name = "Bus Stops\n(per Cell)"
  ) +
  
  # clean design
  theme_minimal() +
  theme(
    panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Bus Transit Accessibility"
  )

# display map
print(bus_map)
ggsave("04.3_raw_bus_density_map.png", bus_map, width = 7, height = 4, dpi = 300)

library(patchwork)
combined_map <- rail_map + bus_map

print(combined_map)

# save the combined figure to a file
# note: width is doubled (20 instead of 10) to accommodate both plots side-by-side
ggsave("04.3_raw_combined_transit_density.png", combined_map, width = 10, height = 4, dpi = 300)

