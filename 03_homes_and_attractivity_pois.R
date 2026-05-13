library(data.table)
# setwd(...)
mob_income <- fread("02_mobility_with_income.csv.gz")

table(mob_income$uid)
table(mob_income$location_type)

# filter for 'home' type and ensure income_group is not NA
valid_home_users <- unique(mob_income[location_type == "home" & !is.na(income_group), uid])

# keep ONLY these valid users in the main dataset
# drops commuters from outside the mapped area and tourists without homes
mob_study_group <- mob_income[uid %in% valid_home_users]

# check how many users we lost vs. kept
cat("Original active users:", uniqueN(mob_income$uid), "\n")
cat("Users with valid homes:", uniqueN(mob_study_group$uid), "\n")

# re-running class assignment on the cleaned group

# extract exact home locations to assign social class
homes <- mob_study_group[location_type == "home" & !is.na(income_group)]
user_social_class <- unique(homes[, .(uid, user_class = income_group)])

library(sf)
library(ggplot2)

# plotting
# 1. load background municipality map
municipality_merged <- readRDS("02_municipality_map_with_income.rds")
# 2. ensure exactly ONE home location per user to avoid overplotting
unique_homes <- unique(homes, by = "uid")
# 3. check if 'lon' and 'lat' are present. if not, merge from grid mapping
if (!"lon" %in% names(unique_homes)) {
  grid_mapping <- fread("02_nagoya_grid_coordinates.csv")
  unique_homes <- merge(unique_homes, grid_mapping[, .(x, y, lon, lat)], by = c("x", "y"), all.x = TRUE)
}
# 4. convert homes data.table to a spatial (sf) object
homes_sf <- st_as_sf(unique_homes[!is.na(lon)], coords = c("lon", "lat"), crs = 4326)
# ensure social class factor has correct logical order
homes_sf$income_group <- factor(homes_sf$income_group, levels = c("Low", "Medium", "High"))

# visualization
home_map <- ggplot() +
  # layer 1: background map of Aichi Prefecture (light gray, clean borders)
  geom_sf(data = municipality_merged, fill = "gray95", color = "white", linewidth = 0.3) +
  
  # layer 2: home locations, colored by social class
  geom_sf(data = homes_sf, aes(color = income_group), size = 0.5, alpha = 0.6) +
  
  # custom color palette
  scale_color_manual(values = c("Low" = "#440154FF",    # Dark purple
                                "Medium" = "#21908CFF", # Teal
                                "High" = "#FDE725FF"),  # Yellow
                     name = "Income Group") +
  
  # increases point size in the legend only
  guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  
  # clean minimalist theme
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5))

# display map
print(home_map)
# save map
ggsave("03_home_locations_class_map.png", home_map, width = 7, height = 5, dpi = 300)


# merge user's social class back into dataset
mob_analysis <- merge(mob_study_group, user_social_class, by = "uid", all.x = TRUE)

total_active_cells <- uniqueN(mob_analysis[, .(x, y)])

# filter for discretionary mobility (third locations) for the mixing matrix
third_places <- mob_analysis[location_type == "third_place" & !is.na(income_group)]
table(mob_analysis$location_type)
table(mob_analysis$income_group)
table(mob_analysis$user_class)
length(unique(mob_analysis$uid))

# create segregation mixing matrix
mixing_matrix <- table(Visitor_Class = third_places$user_class, 
                       Visited_Neighborhood = third_places$income_group)

# print result
print("Mobility Mixing Matrix (Third Locations):")
print(mixing_matrix)

# getting POIs
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)

# 1. fix: bounding box strictly defined based on the grid
grid_spatial <- readRDS("02_grid_spatial.rds")
nagoya_bbox <- st_bbox(grid_spatial)

# 4 POI categories (expanded for robust academic analysis & Japanese context)

# 1. Food & Drink
# expanded: food courts (common in malls), ice cream parlors, etc.
q_food <- opq(bbox = nagoya_bbox) %>% 
  add_osm_feature(key = 'amenity', 
                  value = c('cafe', 'restaurant', 'bar', 'pub', 'fast_food', 
                            'food_court', 'ice_cream', 'biergarten'))

# 2. Shopping
# expanded: drugstores and electronics stores are very important "third places" in Japan. 
# bakeries and bookstores are also great meeting spots.
q_shop <- opq(bbox = nagoya_bbox) %>% 
  add_osm_feature(key = 'shop', 
                  value = c('supermarket', 'mall', 'department_store', 'convenience', 
                            'clothes', 'bakery', 'electronics', 'books', 'drugstore', 
                            'chemist', 'shoes', 'cosmetics', 'sports', 'hardware'))

# 3. Culture & Entertainment
# expanded: Japan-specific entertainment! pachinko arcades and karaoke are core social spaces. 
# added community centers too.
q_culture <- opq(bbox = nagoya_bbox) %>% 
  add_osm_feature(key = 'amenity', 
                  value = c('cinema', 'theatre', 'library', 'arts_centre', 'nightclub', 
                            'amusement_arcade', 'casino', 'karaoke', 'community_centre', 'social_facility'))

# 4. Parks & Leisure
# expanded: playgrounds, gardens (formal gardens matter in Japan), sports pitches, and stadiums.
q_leisure <- opq(bbox = nagoya_bbox) %>% 
  add_osm_feature(key = 'leisure', 
                  value = c('park', 'sports_centre', 'fitness_centre', 'playground', 
                            'garden', 'pitch', 'stadium', 'nature_reserve', 'water_park'))

# download and clean with function
clean_osm_points <- function(query_obj, category_name) {
  # download data
  osm_data <- osmdata_sf(query_obj)$osm_points
  
  if (is.null(osm_data) || nrow(osm_data) == 0) return(NULL)
  
  # ensure it has an ID, keep only geometry, and add category label
  osm_clean <- osm_data %>%
    select(geometry) %>%
    mutate(poi_category = category_name)
  
  return(osm_clean)
}

library(osmdata)
print("Downloading OSM data...")

# set_overpass_url("https://z.overpass-api.de/api/interpreter")
poi_food <- clean_osm_points(q_food, "Food_Drink")
poi_shop <- clean_osm_points(q_shop, "Shopping")
poi_culture <- clean_osm_points(q_culture, "Culture_Entertainment")
poi_leisure <- clean_osm_points(q_leisure, "Parks_Sports")


# combine all POIs into one master spatial object
all_pois <- bind_rows(poi_food, poi_shop, poi_culture, poi_leisure)
# ensure CRS matches the grid (WGS84 / EPSG:4326)
all_pois <- st_transform(all_pois, crs = 4326)
print(table(all_pois$poi_category))

# assign POIs to grid cells
# reverting to st_nearest_feature, works perfectly for points
poi_with_grid <- st_join(all_pois, grid_spatial, join = st_nearest_feature)

# convert to data.table to count how many of EACH category fell into EACH grid cell
setDT(poi_with_grid)

# create wide table: rows are grid cells (x, y), columns are POI type counts
grid_poi_counts <- dcast(poi_with_grid, x + y ~ poi_category, fun.aggregate = length)

# merge POI counts back into main 'grid_with_income' dictionary
grid_nagoya_only <- readRDS("02_grid_nagoya_only.rds")
grid_with_income_poi <- merge(grid_nagoya_only, grid_poi_counts, by = c("x", "y"), all.x = TRUE)

# replace NA counts with 0 
poi_cols <- c("Food_Drink", "Shopping", "Culture_Entertainment", "Parks_Sports")
for (col in poi_cols) {
  if (col %in% names(grid_with_income_poi)) {
    grid_with_income_poi[is.na(get(col)), (col) := 0]
  }
}

# verify final grid (you should see the 4 POI columns here)
print(head(grid_with_income_poi))

# plotting POIs
library(ggplot2)
library(sf)
library(viridis)
library(patchwork)

# plotting with 1 scale
# set common theme for clean mapping
mapping_theme <- theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        strip.text = element_text(size = 11, face = "bold"))

grid_mapping <- fread("02_nagoya_grid_coordinates.csv")
grid_with_income_poi <- merge(grid_with_income_poi, grid_mapping[, .(x, y, lon, lat)], by = c("x", "y"), all.x = TRUE)

# filter out NAs and convert back to spatial (sf) object for plotting
plot_data_poi <- st_as_sf(grid_with_income_poi[!is.na(lon)], coords = c("lon", "lat"), crs = 4326)

# melt wide POI columns into long format for faceted plotting
library(reshape2)
poi_cols_to_plot <- c("Food_Drink", "Shopping", "Culture_Entertainment", "Parks_Sports")
plot_data_long <- melt(plot_data_poi, id.vars = "geometry", measure.vars = poi_cols_to_plot, 
                       variable.name = "POI_Category", value.name = "POI_Count")

library(patchwork)

# plotting with 4 scales
# 1. Food & Drink 
p_food <- ggplot(data = plot_data_poi[plot_data_poi$Food_Drink > 0, ]) +
  geom_sf(aes(color = Food_Drink), size = 1.3, alpha = 0.8) +
  scale_color_viridis_c(option = "magma", direction = -1, name = "Count") +
  mapping_theme +
  labs(title = "Food & Drink")

# 2. Shopping 
p_shop <- ggplot(data = plot_data_poi[plot_data_poi$Shopping > 0, ]) +
  geom_sf(aes(color = Shopping), size = 1.3, alpha = 0.8) +
  scale_color_viridis_c(option = "magma", direction = -1, name = "Count") +
  mapping_theme +
  labs(title = "Shopping")

# 3. Culture & Entertainment 
p_culture <- ggplot(data = plot_data_poi[plot_data_poi$Culture_Entertainment > 0, ]) +
  geom_sf(aes(color = Culture_Entertainment), size = 1.3, alpha = 0.8) +
  scale_color_viridis_c(option = "magma", direction = -1, name = "Count") +
  mapping_theme +
  labs(title = "Culture & Entertainment")

# 4. Parks & Sports 
p_parks <- ggplot(data = plot_data_poi[plot_data_poi$Parks_Sports > 0, ]) +
  geom_sf(aes(color = Parks_Sports), size = 1.3, alpha = 0.8) +
  scale_color_viridis_c(option = "magma", direction = -1, name = "Count") +
  mapping_theme +
  labs(title = "Parks & Sports")

# patchworking maps together
final_poi_map2 <- (p_food | p_shop) / (p_culture | p_parks) +
  plot_annotation(
    title = "Comparative Spatial Distribution of POIs in Nagoya",
    subtitle = "OpenStreetMap data aggregated to the 500x500m mobility grid (Independent Scales)",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 12, hjust = 0.5))
  )

print(final_poi_map2)

# nicer plot: plotting POI categories with grid cells (geom_tile)
library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)

# load and transform municipality map
# 1. load base RDS file
municipality_merged <- readRDS("02_municipality_map_with_income.rds")

# 2. convert to Japanese metric projection (EPSG:2449) to match geom_tile coordinates
municipality_merged_metric <- st_transform(municipality_merged, crs = 2449)

print("Converting POI data to metric cells for plotting...")

# 1. transform POI data to metric projection (EPSG:2449) and extract coordinates
plot_data_poi_metric <- plot_data_poi %>%
  st_transform(crs = 2449) %>%
  st_centroid() # ensure clean point coordinates for tiles

poi_df <- plot_data_poi_metric %>%
  st_drop_geometry() %>%
  mutate(
    X_metric = st_coordinates(plot_data_poi_metric)[, 1],
    Y_metric = st_coordinates(plot_data_poi_metric)[, 2]
  )

# plotting with 4 scales (independent scales)

# 1. Food & Drink 
p_food <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = poi_df %>% filter(Food_Drink > 0), 
            aes(x = X_metric, y = Y_metric, fill = Food_Drink), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 1) +
  scale_fill_viridis_c(option = "magma", direction = -1, end = 0.8, name = "Count") +
  mapping_theme +
  labs(title = "Food & Drink") +
  theme(
    plot.title = element_text(size = 12, face = "plain", hjust = 0.5))

# 2. Shopping 
p_shop <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = poi_df %>% filter(Shopping > 0), 
            aes(x = X_metric, y = Y_metric, fill = Shopping), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 1) +
  scale_fill_viridis_c(option = "magma", direction = -1, end = 0.8, name = "Count") +
  mapping_theme +
  labs(title = "Shopping") +
  theme(
    plot.title = element_text(size = 12, face = "plain", hjust = 0.5))

# 3. Culture & Entertainment 
p_culture <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = poi_df %>% filter(Culture_Entertainment > 0), 
            aes(x = X_metric, y = Y_metric, fill = Culture_Entertainment), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 1) +
  scale_fill_viridis_c(option = "magma", direction = -1, end = 0.8, name = "Count") +
  mapping_theme +
  labs(title = "Culture & Entertainment")  +
  theme(
    plot.title = element_text(size = 12, face = "plain", hjust = 0.5))

# 4. Parks & Sports 
p_parks <- ggplot() +
  geom_sf(data = municipality_merged_metric, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_tile(data = poi_df %>% filter(Parks_Sports > 0), 
            aes(x = X_metric, y = Y_metric, fill = Parks_Sports), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 1) +
  scale_fill_viridis_c(option = "magma", direction = -1, end = 0.8, name = "Count") +
  mapping_theme +
  labs(title = "Parks & Sports") +
  theme(
    plot.title = element_text(size = 12, face = "plain", hjust = 0.5))

# patchworking maps together
final_poi_map2 <- (p_food | p_shop) / (p_culture | p_parks) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 5, hjust = 0.5)
    )
  )

print(final_poi_map2)

# saving final datasets and plots

# 1. save POI distribution map
ggsave("03_poi_distribution_map2.png", final_poi_map2, width = 20, height = 15, dpi = 300)

# 2. save fully enriched grid (coords + income group + POI counts)
# saving as .rds perfectly preserves spatial data structures
saveRDS(grid_with_income_poi, "03_grid_with_pois.rds")

# 3. save comprehensive mobility analysis dataset
# contains every valid trip, user's home class, and visited neighborhood class
saveRDS(mob_analysis, "03_mob_analysis_full.rds")