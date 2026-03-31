# Purpose -------------------------------------------------------------------------------------
# Compare OSM and DVRPC lane counts, prepare interactive map for manually checking data quality,
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

# box_ls(362958210990)

# Speed data
speed <- box_read_rds(2171353657698) %>% 
  mutate(seg_id = as.character(seg_id)) 

# OSM data
osm <- box_read_rds(2174904376082) %>% 
  select(seg_id, lanes_osm = lanes)

# Street network data including DVRPC data
dvrpc <- box_read_rds(2151757279199) %>% 
  select(seg_id, stname, divided_roadway = DIV_RDWY, lanes_dvrpc = NEW_LANES) %>% 
  mutate(divided_roadway = case_when(divided_roadway == 0 ~ "No",
                                     divided_roadway == 1 ~ "Yes")) %>% 
  st_drop_geometry()

# Segment geometry
centerlines <- box_read_rds(2139915462983) %>% 
  mutate(seg_id = as.character(seg_id))

# Overall data --------------------------------------------------------------------------------

lanes_data_all <- centerlines %>% 
  select(seg_id) %>% 
  left_join(dvrpc) %>% 
  left_join(osm) %>% 
  mutate(lanes_diff = lanes_dvrpc - lanes_osm) %>% 
  mutate(in_modeling_data = seg_id %in% speed$seg_id)

# Modeling data extract -----------------------------------------------------------------------

lanes_data_modeling <- lanes_data_all %>% 
  filter(in_modeling_data) %>% 
  select(-in_modeling_data) %>% 
  arrange(lanes_dvrpc, lanes_osm) %>% 
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
  <b>Divided:</b> {divided_roadway}<br/>
  <b>DVRPC:</b> {lanes_dvrpc}<br/>
  <b>OSM:</b> {lanes_osm}
"))

# Divide up task ------------------------------------------------------------------------------

mapview(lanes_data_modeling %>% 
          filter(task_for == "Christine"),
        label = "hypertext",
        color = "purple",
        map.types = "Esri.WorldImagery")

mapview(lanes_data_modeling %>% 
          filter(task_for == "Demi"),
        label = "hypertext",
        color = "lightgreen",
        map.types = "Esri.WorldImagery")

mapview(lanes_data_modeling %>% 
          filter(task_for == "Kavana"),
        label = "hypertext",
        color = "skyblue",
        map.types = "Esri.WorldImagery")

mapview(lanes_data_modeling %>% 
          filter(task_for == "Chi-Hyun"),
        label = "hypertext",
        color = "yellow",
        map.types = "Esri.WorldImagery")

# Write out CSVs for checking -----------------------------------------------------------------

box_output_folder <- 374124673315

lanes_export <- lanes_data_modeling %>% 
  st_drop_geometry() %>%
  mutate(lanes_verified = NA) %>%
  mutate(notes = NA) %>% 
  select(-hypertext)

christine_data <- lanes_export %>% 
  filter(task_for == "Christine")

demi_data <- lanes_export %>% 
  filter(task_for == "Demi")

kavana_data <- lanes_export %>% 
  filter(task_for == "Kavana")

chihyun_data <- lanes_export %>% 
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










