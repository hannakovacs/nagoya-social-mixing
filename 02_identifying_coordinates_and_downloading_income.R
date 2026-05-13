# setwd(...)
# identifying real coordinates based on Gergo
# using code from his github: https://github.com/pintergreg/reverse-engineering-YJMob100K-grid/blob/main/src/generate_grid.ipynb
library(data.table)
library(sf)

# 200x200 empty grid
grid_dt <- as.data.table(expand.grid(x = 1:200, y = 1:200))

# constants from Python script
startx <- -60681.81818181819
starty <- -55555.55555555556
stepx <- 500 / 1.1
stepy <- 500 / 0.9
offsety <- 200 * stepy

# calculate grid indexes (considering inverted vertices)
# j is vertical (x), i is horizontal (y)
grid_dt[, i := y - 1]
grid_dt[, j := x - 1]

# calculate centroid
grid_dt[, X_metric := startx + (i * stepx) + (stepx / 2)]
grid_dt[, Y_metric := starty - offsety + (j * stepy) + (stepy / 2)]

# convert to sf (Japanese EPSG:2449)
grid_sf <- st_as_sf(grid_dt, coords = c("X_metric", "Y_metric"), crs = 2449)

# convert to standard WGS84 
grid_wgs84 <- st_transform(grid_sf, 4326)

# get lon and lat
coords <- st_coordinates(grid_wgs84)
grid_dt[, lon := coords[, 1]]
grid_dt[, lat := coords[, 2]]

# save as csv
grid_mapping <- grid_dt[, .(x, y, lon, lat)]
fwrite(grid_mapping, "02_nagoya_grid_coordinates.csv")

# check if it worked
library(leaflet)

# sample 500 random points from your grid to avoid freezing the viewer
set.seed(42)
sample_points <- grid_mapping[sample(nrow(grid_mapping), 500)]

# create interactive map
leaflet(data = sample_points) %>%
  addTiles() %>%  # add default OpenStreetMap background
  addCircleMarkers(
    ~lon, ~lat, 
    radius = 3, 
    color = "red", 
    stroke = FALSE, 
    fillOpacity = 0.8
  )

#####################################################
# https://www.e-stat.go.jp/en/regional-statistics/ssdsview/municipality 
# H4104_Rent per tatami mat of exclusively residential dwellings[yen] from e-stat
library(readxl)
municipality_data <- read_excel("FEI_CITY_260414053329.xlsx")

######################################
######################################
# connect to shapefile
library(dplyr)

# 1. update directory to the specific Aichi JGD2011 folder
shapefile_dir <- "C:/Egyetem/SDS/THESIS/shapefiles_2"

# find all .shp files in the specific directory
shp_files <- list.files(path = shapefile_dir, 
                        pattern = "\\.shp$", 
                        full.names = TRUE, 
                        recursive = TRUE)

# read and bind shapefiles
shape_list <- lapply(shp_files, st_read)
municipality_small_areas <- bind_rows(shape_list)

# 2. extract 5-digit municipality code from the 11-digit small area KEY_CODE
municipality_small_areas$Area_code <- substr(as.character(municipality_small_areas$KEY_CODE), 1, 5)

# --- 3. THE MAGIC STEP: dissolve tiny street blocks into unified city/town polygons ---

# temporarily disable strict geometric checking (Japanese data can be messy)
sf_use_s2(FALSE)

# clean up the map: fix self-intersecting and invalid polygons
municipality_small_areas <- st_make_valid(municipality_small_areas)

# run the union (might take 1-2 mins)
municipality_shape <- municipality_small_areas %>%
  group_by(Area_code) %>%
  summarize(geometry = st_union(geometry))

# turn S2 back on for accurate intersections
sf_use_s2(TRUE)

# validate the result again
municipality_shape <- st_make_valid(municipality_shape)

# plot
plot(st_geometry(municipality_shape), col = "lightgray", border = "darkgray")
# yay, looks like a unified Aichi without holes! :)

# 4. joining rent to municipality shape
municipality_data$area_code <- as.character(municipality_data$area_code)

# connect themmm
municipality_merged <- left_join(municipality_shape, 
                                 municipality_data, 
                                 by = c("Area_code" = "area_code"))

# calculate terciles (3 equal rent brackets)
municipality_merged$rent <- as.numeric(municipality_merged$rent)
terciles <- quantile(municipality_merged$rent, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

# assign labels
municipality_merged$income_group <- cut(municipality_merged$rent, 
                                        breaks = terciles, 
                                        labels = c("Low", "Medium", "High"), 
                                        include.lowest = TRUE)

###############
# merge real coordinates with cleaned mobility stops
mob_stops <- fread("01_mobility_clean.csv.gz")

grid_spatial <- st_as_sf(grid_mapping[!is.na(lon)], 
                         coords = c("lon", "lat"), 
                         crs = 4326)
saveRDS(grid_spatial, "02_grid_spatial.rds")

sf_use_s2(FALSE)
municipality_merged <- st_make_valid(municipality_merged)
# ensure CRS matches perfectly (WGS84 / EPSG:4326)
municipality_merged <- st_transform(municipality_merged, crs = 4326)

# join grid and municipality (income)
grid_joined <- st_join(grid_spatial, municipality_merged, join = st_intersects)
sf_use_s2(TRUE)

grid_with_income <- as.data.table(grid_joined)

# keep only valid Nagoya coords (drop NAs)
grid_nagoya_only <- grid_with_income[!is.na(income_group)]
saveRDS(grid_with_income, "02_grid_with_income.rds")
saveRDS(grid_nagoya_only, "02_grid_nagoya_only.rds")

# both must be data tables
setDT(mob_stops)
# x and y must be numeric in both 
mob_stops[, x := as.numeric(x)]
mob_stops[, y := as.numeric(y)]
grid_nagoya_only[, x := as.numeric(x)]
grid_nagoya_only[, y := as.numeric(y)]

# add 'income_group' directly to 'mob_stops' table
mob_stops[grid_nagoya_only, on = .(x, y), income_group := i.income_group]

# print distribution across wealth categories
print(mob_stops[!is.na(income_group), .N, by = income_group])
table(mob_stops$income_group)
sum(is.na(mob_stops$income_group))


# saving what's needed
# drop rows outside mapped municipalities (NA)
mobilty_with_income <- mob_stops[!is.na(income_group)]

# save enriched mobility dataset
fwrite(mobilty_with_income, "02_mobility_with_income.csv.gz")

# CROP AND SAVE MUNICIPALITY MAP FOR FUTURE PLOTTING
# calculate bounding box of the grid
mobility_extent <- st_bbox(grid_spatial)

# crop municipality map to grid extent
# no need to read_rds, using the active memory object
municipality_cropped <- st_crop(municipality_merged, mobility_extent)

# save cropped map for future plotting
# NOTE: Changed name to avoid "01_" prefix confusion. This is the master background map.
saveRDS(municipality_cropped, "02_municipality_map_with_income.rds")


# plotting to see if i did it correctly

library(ggplot2)

# create a small sample (100k rows) for quick plotting
set.seed(42)
sample_plot <- mob_stops[sample(.N, 100000)]

# create a new column for plotting based on status
sample_plot[, statusz := fifelse(is.na(income_group), "Missing (Sea/Other Prefecture)", "Valid data (Aichi)")]

# plot points based on grid coords
# in YJMob100K, 'y' is horizontal, 'x' is vertical
ggplot(sample_plot, aes(x = y, y = -x, color = statusz)) +
  geom_point(size = 0.5, alpha = 0.6) +
  scale_color_manual(values = c("Missing (Sea/Other Prefecture)" = "lightgray", 
                                "Valid data (Aichi)" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Geographic distribution of NAs",
       subtitle = "The blue area shows the shape of the downloaded Aichi prefecture",
       x = "Horizontal coordinate (Y)",
       y = "Vertical coordinate (X)")


# create random sample for fast plotting
set.seed(42)
sample_plot <- mob_stops[sample(.N, 100000)]

# add real lon and lat from grid_mapping
sample_plot <- merge(sample_plot, grid_mapping, by = c("x", "y"), all.x = TRUE)

# create status column for coloring
sample_plot[, status := fifelse(is.na(income_group), 
                                "Missing (Sea / Other Prefecture)", 
                                "Valid Data (Aichi)")]

# convert to sf using WGS84
sample_sf <- st_as_sf(sample_plot[!is.na(lon)], coords = c("lon", "lat"), crs = 4326)

# transform to Japanese local projection (EPSG: 2449)
# this removes distortion and provides true real-world shape
sample_sf_jgd <- st_transform(sample_sf, crs = 2449)

# plot map with geom_sf
ggplot(data = sample_sf_jgd) +
  geom_sf(aes(color = status), size = 0.5, alpha = 0.6) +
  scale_color_manual(values = c("Missing (Sea / Other Prefecture)" = "lightgray", 
                                "Valid Data (Aichi)" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Geographical Distribution of the Mobility Grid",
       subtitle = "Projected in EPSG:2449 for true real-world proportions",
       x = "Longitude (Projected)",
       y = "Latitude (Projected)")
# holes remain because we lack rent data for some places (unsurveyed/not aggregated)

#####################################################
# INCOME MAP
library(viridis) # For color palettes

# ensure 'income_group' is a factor with correct order
# (important for the legend)
municipality_merged$income_group <- factor(municipality_merged$income_group, 
                                           levels = c("Low", "Medium", "High", "No Data"))

# create map using cropped data
income_map <- ggplot(data = municipality_cropped) +  # <-- changed here
  # draw polygons and color by income_group
  geom_sf(aes(fill = income_group), color = "white", linewidth = 0.2) +
  
  scale_fill_manual(values = c("Low" = "#440154FF",    # Dark purple
                               "Medium" = "#21908CFF", # Teal
                               "High" = "#FDE725FF",   # Yellow
                               "No Data" = "gray80"),  # Light gray
                    name = "Income Level") +
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5))

print(income_map)
ggsave("02_aichi_income_map.png", income_map, width = 6, height = 4, dpi = 300)