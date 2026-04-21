# Purpose -------------------------------------------------------------------------------------
# Write out CSVs and KML files for adding to dataset. 
# Include a label for all times which need to be checked per segment.
# For segments with more than one measurement year, there should be a row for each year.

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

previous_data <- box_read_csv(2190702126891)

network_bike <- box_read_rds(2178022998565) %>% 
  mutate(bike_lane_type_simple =
           case_when(bike_lane_type %in% c("Advisory Bike Lane", "Sharrow") ~ 
                       "Sharrow",
                     bike_lane_type %in% c("Bus/Bike Lane", "Painted Bike Lane") ~ 
                       "Painted",
                     bike_lane_type %in% c("None", "Unknown") ~ 
                       "None",
                     bike_lane_type %in% c("On-Street Separated Bike Lane", "Raised Separated Bike Lane", "Shared Use Sidepath") ~ 
                       "Separated",
                     is.na(bike_lane_type) ~ "None",
                     .default = "CHECK"))

speed <- box_read_rds(2193174265310) %>% 
  mutate(seg_id = as.character(seg_id)) %>% 
  mutate(year = year(speed_measurement_date))

modeling_data <- box_read_rds(2197920559067)

centerlines <- box_read_rds(2139915462983) %>% 
  mutate(seg_id = as.character(seg_id))

# Segment by year coverage --------------------------------------------------------------------

segment_by_year <- speed %>% 
  # distinct: removed 8,580 rows (95%), 472 rows remaining
  distinct(seg_id, year) %>% 
  # filter: no rows removed
  filter(seg_id %in% modeling_data$seg_id) %>% 
  # Join in existing bike data to get years where bike lane status changed by segment
  left_join(network_bike %>% 
              select(seg_id, year, bike_lane_type_historical = bike_lane_type_simple),
            by = c("seg_id", "year")) %>% 
  mutate(bike_lane_type_historical = replace_na(bike_lane_type_historical, "None")) %>% 
  left_join(network_bike %>% 
              filter(year == 2025) %>% 
              select(seg_id, bike_lane_type_current = bike_lane_type_simple),
            by = c("seg_id")) %>% 
  mutate(bike_lane_type_current = replace_na(bike_lane_type_current, "None")) %>% 
  mutate(check_year = if_else(bike_lane_type_current != bike_lane_type_historical, year, NA)) %>% 
  group_by(seg_id, bike_lane_type_historical) %>% 
  # slice_max (grouped): removed 26 rows (6%), 446 rows remaining (removed 0 groups, 446 groups remaining)
  slice_max(order_by = year, with_ties = FALSE) %>% 
  ungroup()

# Join to previous data check data ------------------------------------------------------------

data_output <- previous_data %>% 
  mutate(seg_id = as.character(seg_id)) %>% 
  left_join(segment_by_year %>% 
              select(seg_id, check_year), 
            by = "seg_id") %>% 
  select(-year) %>% 
  mutate(curb_to_curb_width = NA,
         traffic_lanes_width = NA,
         shoulder_width = NA) %>% 
  relocate(notes, .after = everything()) %>% 
  # Reallocate Kavana's segments
  mutate(task_for = case_when(task_for != "Kavana" ~ task_for,
                              between(row, 225, 224 + 37 * 1) ~ "Christine",
                              between(row, 224 + 37 * 1, 224 + 37 * 2) ~ "Demi",
                              between(row, 224 + 37 * 2, 224 + 37 * 3 + 1) ~ "Chi-Hyun"))

# Mapping data --------------------------------------------------------------------------------

data_mapping <- data_output %>% 
  left_join(centerlines %>% select(seg_id)) %>% 
  st_as_sf(crs = "EPSG:4326")

# st_write(data_mapping %>% filter(task_for == "Christine") %>% select(row), "data_check_christine.kml")
# st_write(data_mapping %>% filter(task_for == "Demi") %>% select(row), "data_check_demi.kml")
# # st_write(data_mapping %>% filter(task_for == "Kavana"), "data_check_kavana.kml")
# st_write(data_mapping %>% filter(task_for == "Chi-Hyun") %>% select(row), "data_check_chihyun.kml")

# Export --------------------------------------------------------------------------------------

christine_data <- data_output %>% 
  filter(task_for == "Christine")

demi_data <- data_output %>% 
  filter(task_for == "Demi")

# kavana_data <- data_output %>% 
#   filter(task_for == "Kavana")

chihyun_data <- data_output %>% 
  filter(task_for == "Chi-Hyun")

# box_output_folder <- 374124673315
# 
# box_write(christine_data,
#           file_name = "data_additions_for_christine.csv",
#           dir_id = box_output_folder)
# 
# box_write(demi_data,
#           file_name = "data_additions_for_demi.csv",
#           dir_id = box_output_folder)
# 
# # box_write(kavana_data,
# #           file_name = "data_additions_for_kavana.csv",
# #           dir_id = box_output_folder)
# 
# box_write(chihyun_data,
#           file_name = "data_additions_for_chihyun.csv",
#           dir_id = box_output_folder)










