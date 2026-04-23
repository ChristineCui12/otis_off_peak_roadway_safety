# Purpose -------------------------------------------------------------------------------------
# Compare OSM and DVRPC lane counts and other roadway characteristics, 
# prepare interactive map for manually checking data quality,
# and write out CSV for reporting.

# Preliminaries -------------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(janitor)
library(scales)
library(boxr)
library(sf)
library(mapview)

# Set up remote data access via Box API
box_auth()
box_raw_data_folder <- 362958311858
box_processed_data_folder <- 362958210990

# Read data -----------------------------------------------------------------------------------

# Modeling data
modeling_data <- box_read_rds(2183528646387)

# Segment geometry
centerlines <- box_read_rds(2139915462983) %>% 
  mutate(seg_id = as.character(seg_id))

# Modeling data extract -----------------------------------------------------------------------

data_input <- modeling_data %>% 
  mutate(year = year(speed_measurement_date)) %>% 
  distinct(seg_id, year, lanes, divided_roadway, bike_lane_type_simple, parking, sidewalk_status) %>% 
  # slice_max: removed 26 rows (6%), 446 rows remaining
  slice_max(order_by = year, by = seg_id, with_ties = FALSE)

data_mapping <- data_input %>% 
  left_join(centerlines %>% select(seg_id, stname)) %>% 
  st_as_sf(crs = "EPSG:4326") %>% 
  # Get long/lat of first point in geometry so segments can be sorted by position
  mutate(
    longitude = sapply(geometry, \(g) st_coordinates(g)[1, "X"]),
    latitude  = sapply(geometry, \(g) st_coordinates(g)[1, "Y"])
  ) %>% 
  mutate(latitude = round(latitude, 2),
         longitude = round(longitude, 2)) %>% 
  arrange(desc(latitude), longitude) %>% 
  mutate(row = row_number(), .before = everything()) %>% 
  mutate(task_for =
           case_when(row <= 112 ~ "Christine",
                     row <= 224 ~ "Demi",
                     row <= 336 ~ "Kavana",
                     .default = "Chi-Hyun"),
         .after = row) %>% 
  mutate(hypertext =  glue::glue("
  <b>Row number:</b> {row}<br/>
  <b>Street:</b> {stname}<br/>
  <b>Lanes:</b> {lanes}<br/>
  <b>Divided:</b> {divided_roadway}<br/>
  <b>Bike lane:</b> {bike_lane_type_simple}<br/>
  <b>ParkingC:</b> {parking}<br/>
  <b>Sidewalk:</b> {sidewalk_status}
"))

# Divide up task ------------------------------------------------------------------------------

map <- mapview(data_mapping,
               label = "hypertext",
               zcol = "task_for",
               color = c("red", "green", "magenta", "turquoise2"),
               map.types = c("Esri.WorldImagery", "CartoDB.Positron"))

mapshot2(map, "data_check_map.html")

st_write(data_mapping, "data_check_all.kml")
st_write(data_mapping %>% filter(task_for == "Christine"), "data_check_christine.kml")
st_write(data_mapping %>% filter(task_for == "Demi"), "data_check_demi.kml")
st_write(data_mapping %>% filter(task_for == "Kavana"), "data_check_kavana.kml")
st_write(data_mapping %>% filter(task_for == "Chi-Hyun"), "data_check_chihyun.kml")

# Write out CSVs for checking -----------------------------------------------------------------

box_output_folder <- 374124673315

data_export <- data_mapping %>% 
  st_drop_geometry() %>%
  mutate(notes = NA) %>% 
  select(-c(hypertext, longitude, latitude)) %>% 
  relocate(stname, .after = seg_id)

christine_data <- data_export %>% 
  filter(task_for == "Christine")

demi_data <- data_export %>% 
  filter(task_for == "Demi")

kavana_data <- data_export %>% 
  filter(task_for == "Kavana")

chihyun_data <- data_export %>% 
  filter(task_for == "Chi-Hyun")

# box_write(christine_data,
#           file_name = "lanes_check_for_christine.csv",
#           dir_id = box_output_folder)
# 
# box_write(demi_data,
#           file_name = "lanes_check_for_demi.csv",
#           dir_id = box_output_folder)
# 
# box_write(kavana_data,
#           file_name = "lanes_check_for_kavana.csv",
#           dir_id = box_output_folder)
# 
# box_write(chihyun_data,
#           file_name = "lanes_check_for_chihyun.csv",
#           dir_id = box_output_folder)










