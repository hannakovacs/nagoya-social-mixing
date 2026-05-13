#setwd("...")

# load mobility data
library(data.table)
library(ggplot2)

file_path <- "yjmob100k-dataset1.csv.gz"
mob_data <- fread(file_path)
length(unique(mob_data$uid)) # meaning that there are 100 000 different people
table(mob_data$t) # 1 time slot = 30 minutes, values: 0-47
# 00:00-00:30 is t=0. 08:00 is t=16. 17:00 is t=33. 21:00 is t=42.

# finding weekends
mob_data[, dow_proxy := d %% 7]

# check total activity volume per day proxy
volume_check <- mob_data[, .(total_records = .N), by = dow_proxy]
setorder(volume_check, dow_proxy) # sort to easily inspect differences
print(volume_check)

# plot total volume to spot the two days with the lowest activity
ggplot(volume_check, aes(x = factor(dow_proxy), y = total_records)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Total activity by day proxy",
       x = "Day proxy (d %% 7)",
       y = "Total records")

# check the daily rhythm for commuting peaks
rhythm_check <- mob_data[, .(records = .N), by = .(dow_proxy, t)]

# plot temporal distribution for each day
ggplot(rhythm_check, aes(x = t, y = records, color = factor(dow_proxy))) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "B", begin = 0.3, end = 0.9) + 
  theme_minimal() +
  facet_wrap(~ dow_proxy) +
  labs(x = "Time slot (1-48)",
       y = "Total records") +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0.5))

# based on the plots, 0 and 6 will be the weekends
mob_data[, is_weekend := (d %% 7) %in% c(0, 6)]

# identify unique home locations per user
night_stops <- mob_data[t %in% c(42:47, 0:11)] # stops between 9 PM and 6 AM
home_locations <- night_stops[, .N, by = .(uid, x, y)][order(uid, -N)]
home_locations <- home_locations[, head(.SD, 1), by = uid] # most frequent night location
setnames(home_locations, c("x", "y"), c("home_x", "home_y")) # rename for merging

# identify unique work locations per user
work_stops <- mob_data[t %in% 17:34 & is_weekend == FALSE] # stops between 8 AM and 5 PM on weekdays
work_locations <- work_stops[, .N, by = .(uid, x, y)][order(uid, -N)]
work_locations <- work_locations[, head(.SD, 1), by = uid] # most frequent work location
setnames(work_locations, c("x", "y"), c("work_x", "work_y"))

# merge home and work coordinates back to the main dataset
mob_data <- merge(mob_data, home_locations[, .(uid, home_x, home_y)], by = "uid", all.x = TRUE)
mob_data <- merge(mob_data, work_locations[, .(uid, work_x, work_y)], by = "uid", all.x = TRUE)

# assign location types
mob_data[, location_type := "third_place"]
mob_data[x == home_x & y == home_y, location_type := "home"]
mob_data[x == work_x & y == work_y, location_type := "work"]

# cleanup temp columns
mob_data[, c("home_x", "home_y", "work_x", "work_y") := NULL]


## checking if it worked
print(mob_data[, .N, by = location_type]) # overall distribution of location types

# verify that each user has exactly 1 home and 1 work location
home_check <- mob_data[location_type == "home", .(unique_homes = uniqueN(paste(x, y))), by = uid]
work_check <- mob_data[location_type == "work", .(unique_workplaces = uniqueN(paste(x, y))), by = uid]

print(home_check[unique_homes > 1])
print(work_check[unique_workplaces > 1])
# empty tables mean no one has multiple home or work locations

# visualize temporal distribution by location type
temporal_dist <- mob_data[, .N, by = .(t, location_type)]

ggplot(temporal_dist, aes(x = t, y = N, color = location_type)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Temporal distribution of location types",
       x = "Time slot (1-48)",
       y = "Number of records",
       color = "Location type")

# too many third places, let's see what are real stops:
setorder(mob_data, uid, d, t) # sort chronologically

# check if the user stayed in the same grid cell as the previous or next time slot
mob_data[, is_stop := (x == shift(x, type = "lag") & y == shift(y, type = "lag")) |
           (x == shift(x, type = "lead") & y == shift(y, type = "lead")), 
         by = .(uid, d)]

# keep only actual stops
mob_stops <- mob_data[is_stop == TRUE | is.na(is_stop)]

# check temporal distribution again on clean data
temporal_dist_clean <- mob_stops[, .N, by = .(t, location_type)]

ggplot(temporal_dist_clean, aes(x = t, y = N, color = location_type)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Temporal distribution of location types (stops only)",
       x = "Time slot (0-47)",
       y = "Number of records",
       color = "Location type")

# save it
fwrite(mob_stops, "01_mobility_clean.csv.gz")

# check third place reduction ratio
all_third_places <- nrow(mob_data[location_type == "third_place"])
real_third_places <- nrow(mob_stops[location_type == "third_place"])
(all_third_places - real_third_places) / all_third_places

total_counts <- temporal_dist_clean[, .(Total_N = sum(N)), by = location_type]
print(total_counts)