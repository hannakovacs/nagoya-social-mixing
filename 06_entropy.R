# step 1-3: setup, profiling & categorization

#setwd(...)
library(data.table)

# load data (skip if already in memory)
mob_data <- fread("02_mobility_with_income.csv.gz")
grid_master <- readRDS("05.1_grid_MASTER_SMOOTHED.rds") 
setDT(grid_master)

# profiling (run only once!)
setnames(mob_data, "income_group", "location_income")
user_profiles <- unique(mob_data[location_type == "home" & !is.na(location_income), 
                                 .(uid, user_income = location_income)])
mob_data <- merge(mob_data, user_profiles, by = "uid", all.x = TRUE)
mob_data <- mob_data[!is.na(user_income)]


# step 4: defining temporal-spatial categories

print("Categorizing stops...")

# label EVERY stop based on thesis logic
mob_data[, activity_type := fcase(
  # 1. work (daytime, weekday at workplace)
  location_type == "work", "Work",
  
  # 2. weekday evening (third place, weekday 17:00 - 22:00)
  location_type == "third_place" & is_weekend == FALSE & t >= 34 & t < 44, "Evening",
  
  # 3. weekend (third place, all weekend)
  location_type == "third_place" & is_weekend == TRUE, "Weekend",
  
  # 4. weekday daytime leisure (third place, weekday 08:00 - 17:00)
  location_type == "third_place" & is_weekend == FALSE & t >= 16 & t < 34, "Daytime_Leisure",
  
  default = "Other"
)]

# filter out home and other (nighttime third place) stops
analysis_stops <- mob_data[activity_type != "Other"]


# step 5: calculating entropy by category
# entropy function (run only once)
entropy_func <- function(p1, p2, p3) {
  p <- c(p1, p2, p3)
  p <- p[p > 0] 
  if (length(p) == 0) return(0)
  return(-sum(p * log(p)) / log(3)) 
}

# calculate per cell AND per category (long format)
mixing_long <- analysis_stops[, .(
  Total_Visitors = .N,
  p_Low = sum(user_income == "Low") / .N,
  p_Med = sum(user_income == "Medium") / .N,
  p_High = sum(user_income == "High") / .N
), by = .(x, y, activity_type)]

# calculate index
mixing_long[, Entropy := mapply(entropy_func, p_Low, p_Med, p_High)]

# noise filtering: only calculate entropy where there were at least 15 visitors in the given category
mixing_long <- mixing_long[Total_Visitors >= 15]


# step 6: pivot to wide format (the magic trick)

print("Pivoting to wide format (dcast)...")

# dcast reshapes the data to be perfect for regression
# creates Entropy_Work, Entropy_Evening, etc., and Total_Visitors_Work... columns
mixing_wide <- dcast(mixing_long, 
                     x + y ~ activity_type, 
                     value.var = c("Entropy", "Total_Visitors"),
                     fill = NA) # stays NA where visitor count was too low

# +1 bonus: calculate 'discretionary total' (average of weekend + evening) if needed
mixing_wide[, Entropy_Leisure_Total := rowMeans(.SD, na.rm = TRUE), .SDcols = c("Entropy_Evening", "Entropy_Weekend")]


# step 7: merge with master grid & save

library(dplyr)
library(sf)

# 1. load original, UNTOUCHED spatial sf object
# (not the data.table version, but the clean sf file with intact geometry)
grid_spatial_sf <- readRDS("05.1_grid_MASTER_SMOOTHED.rds") %>%
  # double check to ensure no duplicates here either
  distinct(x, y, .keep_all = TRUE)

# 2. convert mixing_wide to data.frame so it plays nicely with sf
mixing_df <- as.data.frame(mixing_wide)

# 3. safe join based on 'x' and 'y' columns
# left_join attaches to an sf object, keeping the output as a flawless sf object (with polygons)
final_regression_data <- grid_spatial_sf %>%
  left_join(mixing_df, by = c("x", "y"))

# 4. (optional check) verify CRS and geometry
print(st_geometry_type(final_regression_data)[1])
print(st_crs(final_regression_data))

# 5. save
saveRDS(final_regression_data, "06_FINAL_REGRESSION_MASTER.rds")


# step 8: mapping temporal entropy dynamics

library(ggplot2)
library(dplyr)
library(tidyr)


# 1. load data (if not in memory)
final_data <- readRDS("06_FINAL_REGRESSION_MASTER.rds")
municipality_map <- readRDS("02_municipality_map_with_income.rds")

# 2. prepare data for multi-panel plotting (faceting)
plot_data_long <- final_data %>%
  st_drop_geometry() %>% # safety step so pivot_longer doesn't trip over geometry
  # select only our 4 main categories for mapping
  select(x, y, lon, lat, Entropy_Work, Entropy_Daytime_Leisure, Entropy_Evening, Entropy_Weekend) %>%
  # pivot table from wide to long format
  pivot_longer(
    cols = starts_with("Entropy_"),
    names_to = "Activity_Type",
    values_to = "Entropy_Score"
  ) %>%
  # keep only cells with meaningful (greater than 0) data
  filter(!is.na(Entropy_Score) & Entropy_Score > 0)

# 3. 'beautify' category names for map labels
plot_data_long <- plot_data_long %>%
  mutate(Activity_Type = case_when(
    Activity_Type == "Entropy_Work" ~ "Work",
    Activity_Type == "Entropy_Daytime_Leisure" ~ "Daytime Leisure",
    Activity_Type == "Entropy_Evening" ~ "Evening Leisure",
    Activity_Type == "Entropy_Weekend" ~ "Weekend",
    TRUE ~ Activity_Type
  ))

# 4. convert back to spatial (sf) object using lon/lat columns
plot_sf <- st_as_sf(plot_data_long, coords = c("lon", "lat"), crs = 4326)


# 4-panel faceted map generation

# 1. prepare data for cell (tile) plotting

# transform background map and points to Japanese metric projection (EPSG:2449)
# this gives us distortion-free reality and lets us calculate in meters
municipality_map_metric <- st_transform(municipality_map, crs = 2449)
plot_sf_metric <- st_transform(plot_sf, crs = 2449)

# extract coordinates into a simple dataframe for geom_tile()
plot_df <- plot_sf_metric %>%
  st_drop_geometry() %>%
  mutate(
    X_metric = st_coordinates(plot_sf_metric)[, 1],
    Y_metric = st_coordinates(plot_sf_metric)[, 2]
  )

# 2. generate 4-panel faceted map (with cells)
faceted_entropy_map <- ggplot() +
  # background: Nagoya outlines
  geom_sf(data = municipality_map_metric, fill = "gray96", color = "white", linewidth = 0.2) +
  
  # foreground: perfect cells instead of points (geom_tile)
  # width and height come from your original grid generation code
  # since we use fill, there are no overlapping lines
  geom_tile(data = plot_df, aes(x = X_metric, y = Y_metric, fill = Entropy_Score), 
            width = 500 / 1.1, height = 500 / 0.9, alpha = 0.9) +
  
  # color scale: ATTENTION! 'color' parameter replaced with 'fill'
  scale_fill_viridis_c(
    option = "B", 
    direction = -1, 
    name = "Social Mixing Index\n(Shannon Entropy)",
    limits = c(0, 1), # fix the scale for all 4 panels
    na.value = "transparent"
  ) +
  
  # split into 4 panels
  facet_wrap(~ Activity_Type, ncol = 2) +
  
  # clean academic design
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold", color = "gray20"), # panel titles
    panel.spacing = unit(1.5, "lines"), 
    plot.background = element_rect(fill = "white", color = NA)
  )

# display and save
print(faceted_entropy_map)
ggsave("06_faceted_entropy_dynamics_map_CELLS.png", faceted_entropy_map, width = 9, height = 6, dpi = 300, bg = "white")


# bar chart: valid grid cells for regression
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. load the final, filtered regression database
final_data <- readRDS("06_FINAL_REGRESSION_MASTER.rds")

# 2. calculate the number of valid cells (with Entropy values) per category
# sf_drop_geometry() is used to treat the spatial object as a standard data frame
bar_data_cells <- final_data %>%
  st_drop_geometry() %>%
  summarise(
    # count only non-NA values (i.e., cells that met the >= 15 visitors threshold)
    Work = sum(!is.na(Entropy_Work)),
    Daytime_Leisure = sum(!is.na(Entropy_Daytime_Leisure)),
    Evening = sum(!is.na(Entropy_Evening)),
    Weekend = sum(!is.na(Entropy_Weekend))
  ) %>%
  # pivot to long format for visualization purposes
  pivot_longer(cols = everything(), names_to = "Activity", values_to = "Valid_Cells")

# 3. add clean, multi-line labels for the X-axis categories
bar_data_cells <- bar_data_cells %>%
  mutate(Activity_Label = case_when(
    Activity == "Work" ~ "1. Work\n(Weekday Daytime)",
    Activity == "Daytime_Leisure" ~ "2. Leisure\n(Weekday Daytime)",
    Activity == "Evening" ~ "3. Leisure\n(Weekday Evening)",
    Activity == "Weekend" ~ "4. Weekend\n(All Day)"
  )) %>%
  arrange(Activity_Label) # ensure proper chronological/categorical order (1-4)

# 4. create the bar chart using ggplot2
valid_cells_barplot <- ggplot(bar_data_cells, aes(x = Activity_Label, y = Valid_Cells, fill = Activity_Label)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.85, show.legend = FALSE) +
  
  # display exact counts on top of the bars with comma separators
  geom_text(aes(label = format(Valid_Cells, big.mark = ",", scientific = FALSE)),
            vjust = -0.8, size = 4.5, fontface = "bold", color = "gray20") + 
  
  # apply an elegant color scale (mako from viridis)
  scale_fill_viridis_d(option = "B", begin = 0.3, end = 0.8) + 
  
  # format Y-axis to include comma separators and add top margin for text labels
  scale_y_continuous(labels = scales::comma_format(), 
                     expand = expansion(mult = c(0, 0.15))) + 
  
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(), # remove vertical gridlines for a cleaner look
    axis.text.x = element_text(size = 11, face = "bold", color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.title.x = element_blank(), # omit X-axis title as categories are self-explanatory
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5, margin = margin(b = 20)),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(y = "Valid Grid Cells (Count)"
  )

# display and save the plot
print(valid_cells_barplot)
ggsave("06_valid_cells_for_regression_barplot.png", valid_cells_barplot, width = 8, height = 3, dpi = 300, bg = "white")

