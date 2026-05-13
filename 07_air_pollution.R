# master script: air quality analysis (AAQI) & spatial grid

library(tidyverse)
library(sf)
library(gstat)
library(RColorBrewer)

# 0. settings & paths
#setwd(...)
folder_path <- "tenbou"
master_folder <- "tenbou/station_data/"

# 1. column definitions
en_colnames <- c("Year", "Item_Type_Code", "Item_Code_Num", "Item_Code_Alphanum", "Method_Code",
                 "Pref_Code", "Pref_Name", "Pref_Romaji", "City_Code", "City_Name", 
                 "City_Romaji", "Station_Code", "Station_Name", "Station_Romaji", "Station_Category_Code",
                 "Station_Type_Code", "Land_Use_Code", "Land_Use_Name", "Schedule_3_Category", "Valid_Days_Year",
                 "Total_Hours_Year", "Annual_Mean_ppm", "Hours_Over_0.1ppm", "Ratio_Over_0.1ppm_Percent", "Days_Over_0.04ppm",
                 "Ratio_Over_0.04ppm_Percent", "Max_Hourly_Value_ppm", "Day_Mean_2_Percent_Excl_ppm", "Consecutive_Over_0.04ppm", "Days_Over_0.04ppm_LongTerm",
                 "Measurement_Method", "Annual_Item_13", "Annual_Item_14", "Annual_Item_15", "Annual_Item_16")

months <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
all_en_colnames <- c(en_colnames, paste0("Valid_Days_", months), paste0("Hours_", months), paste0("Mean_ppm_", months),
                     paste0("Hours_Over_0.1ppm_", months), paste0("Days_Over_0.04ppm_", months), 
                     paste0("Max_Hourly_ppm_", months), paste0("Max_Daily_ppm_", months),
                     paste0("Monthly_Item_8_", months), paste0("Monthly_Item_9_", months))

# 2. data ingestion functions
process_air_file <- function(file) {
  data <- read_csv(file, locale = locale(encoding = "Shift-JIS"), col_types = cols(.default = "c"), show_col_types = FALSE)
  colnames(data) <- all_en_colnames
  pollutant_code <- str_sub(basename(file), 7, 8)
  data %>% mutate(Pollutant_ID = pollutant_code) %>% 
    select(Station_Code, Station_Name, City_Name, Pref_Name, Pollutant_ID, Annual_Mean_ppm)
}

read_master_file <- function(file) {
  data <- read_csv(file, locale = locale(encoding = "Shift-JIS"), col_types = cols(.default = "c"), col_names = FALSE, show_col_types = FALSE)
  data %>% select(Station_Code = X2, Lat_Deg = X10, Lat_Min = X11, Lat_Sec = X12, Lon_Deg = X13, Lon_Min = X14, Lon_Sec = X15) %>%
    mutate(Station_Code = str_trim(Station_Code)) %>%
    filter(str_detect(Station_Code, "^[0-9]{8}$")) %>%
    mutate(across(-Station_Code, as.numeric)) %>%
    mutate(Latitude = Lat_Deg + (Lat_Min / 60) + (Lat_Sec / 3600),
           Longitude = Lon_Deg + (Lon_Min / 60) + (Lon_Sec / 3600)) %>%
    select(Station_Code, Latitude, Longitude)
}

# 3. core processing
# read pollutants
files <- list.files(path = folder_path, pattern = "*.txt", full.names = TRUE)
aaqi_prep <- files %>% map_df(~process_air_file(.x)) %>%
  mutate(Annual_Mean_ppm = as.numeric(Annual_Mean_ppm)) %>%
  group_by(Station_Code, Station_Name, City_Name, Pref_Name, Pollutant_ID) %>%
  summarise(Value = mean(Annual_Mean_ppm, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Pollutant_ID, values_from = Value, names_prefix = "Pollutant_")

# calculate AAQI (Kyrkilis method, WHO 2021 limits)
limit_NO2 <- 10; limit_PM25 <- 5; limit_O3 <- 60
aaqi_final <- aaqi_prep %>%
  drop_na(Pollutant_03, Pollutant_06, Pollutant_12) %>%
  mutate(NO2_ugm3 = (Pollutant_03 * 46.01) / 0.02445,
         O3_ugm3  = (Pollutant_06 * 48.00) / 0.02445,
         PM25_ugm3 = Pollutant_12,
         AAQI = sqrt((NO2_ugm3/limit_NO2)^2 + (PM25_ugm3/limit_PM25)^2 + (O3_ugm3/limit_O3)^2))

# join with coordinates
master_files <- list.files(path = master_folder, pattern = "*.txt", full.names = TRUE)
station_coords <- master_files %>% map_df(~read_master_file(.x))
aaqi_map_data <- aaqi_final %>% mutate(Station_Code = str_trim(Station_Code)) %>%
  left_join(station_coords, by = "Station_Code") %>% filter(!is.na(Latitude) & !is.na(Longitude))


# 3.5 visualize raw station locations before interpolation

# load and transform the municipality map for the background
municipality_map <- readRDS("02_municipality_map_with_income.rds")
municipality_map <- st_transform(municipality_map, 4326)

# create a spatial object from the final station data
aaqi_sf <- st_as_sf(aaqi_map_data, coords = c("Longitude", "Latitude"), crs = 4326)

# plot raw station locations colored by their calculated AAQI
station_raw_plot <- ggplot() +
  # background map
  geom_sf(data = municipality_map, fill = "gray95", color = "white", linewidth = 0.3) +
  # station points
  geom_sf(data = aaqi_sf, aes(color = AAQI), size = 3, alpha = 0.9) +
  # color scale matching the final map's theme
  scale_color_viridis_c(
    option = "magma", 
    direction = -1, 
    name = "Raw AAQI\nValue"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Air Quality Monitoring Stations",
    subtitle = "Calculated AAQI values prior to spatial interpolation",
    caption = "Data: Tenbou"
  )

print(station_raw_plot)

# calculate the bounding box of the base municipality map
bbox <- st_bbox(municipality_map)

station_raw_plot_zoomed <- ggplot() +
  # background map
  geom_sf(data = municipality_map, fill = "gray95", color = "white", linewidth = 0.3) +
  # station points (slightly smaller with a subtle border for better visibility)
  geom_sf(data = aaqi_sf, aes(fill = AAQI), size = 2.5, shape = 21, color = "gray30", alpha = 0.9) +
  # color scale applied to 'fill' since we use shape 21
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1, 
    name = "Raw AAQI\nValue"
  ) +
  # limit map view to municipality boundaries with a small 0.05 degree buffer
  coord_sf(
    xlim = c(bbox["xmin"] - 0.05, bbox["xmax"] + 0.05),
    ylim = c(bbox["ymin"] - 0.05, bbox["ymax"] + 0.05),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "gray90", linetype = "dashed"), # subtle gridlines
    axis.text = element_text(color = "gray50", size = 8), # show coordinates lightly
    axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right"
  ) 

print(station_raw_plot_zoomed)
ggsave("07_aaqi_stations_raw_zoomed.png", station_raw_plot_zoomed, width = 10, height = 8, dpi = 300)


# 4. spatial interpolation (IDW)

grid_spatial <- readRDS("02_grid_spatial.rds")
aaqi_sf <- st_as_sf(aaqi_map_data, coords = c("Longitude", "Latitude"), crs = 4326)

# interpolate onto grid centroids
aaqi_metric <- st_transform(aaqi_sf, crs = 2449)
grid_metric <- st_transform(grid_spatial, crs = 2449)
grid_centroids <- st_centroid(grid_metric)

idw_result <- idw(formula = AAQI ~ 1, locations = aaqi_metric, newdata = grid_centroids, idp = 2.0)

# normalization and inversion in a shared step
normalize_minmax <- function(x) {
  if(all(x == 0) || max(x, na.rm=TRUE) == min(x, na.rm=TRUE)) return(rep(0, length(x)))
  return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
}

# 1. extract raw IDW values
raw_pollution <- idw_result$var1.pred
# 2. normalize to 0-1 range
final_environmental_score <- (normalize_minmax(raw_pollution))


# 5. seamless grid reconstruction (for mapping)

# inserting the newly normalized scores
seamless_grid <- st_make_grid(grid_metric, n = c(200, 200), square = TRUE) %>% st_sf()
grid_final_smooth <- st_join(seamless_grid, 
                             st_sf(AAQI_Score = final_environmental_score, geometry = st_geometry(grid_centroids)), 
                             join = st_nearest_feature)

# 6. master data merge for regression (for modeling)

# applying the newly normalized scores here too
grid_master <- readRDS("06_FINAL_REGRESSION_MASTER.rds") %>%
  mutate(AAQI_Score = final_environmental_score)
saveRDS(grid_master, "07_grid_MASTER_FOR_REGRESSION.rds")


# 7. final visualization

municipality_map <- readRDS("02_municipality_map_with_income.rds")
grid_final_smooth <- st_transform(grid_final_smooth, 4326)
municipality_map <- st_transform(municipality_map, 4326)

final_overlay_plot <- ggplot() +
  geom_sf(data = municipality_map, fill = "gray92", color = "gray75", linewidth = 0.1) +
  geom_sf(data = grid_final_smooth, aes(fill = AAQI_Score), color = NA, alpha = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Environmental Quality\n(0=Clean,1=Smog)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())
print(final_overlay_plot)
ggsave("07_aaqi_final_overlay.png", final_overlay_plot, width = 6, height = 4, dpi = 300)