# Purpose -------------------------------------------------------------------------------------
# Join Openstreetmap roadway characteristics data with Philadelphia street segments

# Preliminaries -------------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(janitor)
library(sf)
library(boxr)
library(mapview)

# Set up remote data access via Box API
box_auth()
box_raw_data_folder <- 362958311858
box_processed_data_folder <- 362958210990

# Read data -----------------------------------------------------------------------------------

# OSM data
osm_raw <- box_read_rds(2138013253852) %>% 
  st_transform("EPSG:2272")

# Street segments
streets_raw <- box_read_rds(2151757279199) %>% 
  st_transform("EPSG:2272")

# Philadelphia mask
phila_mask <- tigris::counties("PA") %>% 
  filter(NAME == "Philadelphia") %>% 
  st_transform("EPSG:2272") %>% 
  # Extend by 20-ft buffer
  st_buffer(dist = 20)

# Test for relevant columns -------------------------------------------------------------------

# # Bus lane coverage very spotty, not reliable
# mapview(osm_raw %>% 
#                    filter(!is.na(bus_lanes)),
#                  zcol = "bus_lanes") +
#   mapview(osm_raw %>% 
#                      filter(!is.na(bus_lanes_forward)),
#                    zcol = "bus_lanes_forward") +
#   mapview(osm_raw %>% 
#                      filter(!is.na(bus_lanes_backward)),
#                    zcol = "bus_lanes_backward")

# # Parking coverage incomplete, but usable
# mapview(osm_raw %>%
#                    filter(!is.na(parking_left)),
#                  zcol = "parking_left")
# 
# mapview(osm_raw %>%
#                    filter(!is.na(parking_right)),
#                  zcol = "parking_right")
# 
# mapview(osm_raw %>%
#                    filter(!is.na(parking_both)),
#                  zcol = "parking_both")

# # Sidewalks; OK coverage split across 4 variables.
# # Will be more useful to construct variable for *no* sidewalk
# mapview(osm_raw %>%
#                    filter(!is.na(sidewalk)),
#                  zcol = "sidewalk")
# 
# mapview(osm_raw %>%
#                    filter(!is.na(sidewalk_right)),
#                  zcol = "sidewalk_right")
# 
# mapview(osm_raw %>%
#                    filter(!is.na(sidewalk_left)),
#                  zcol = "sidewalk_left")
# 
# mapview(osm_raw %>%
#                    filter(!is.na(sidewalk_both)),
#                  zcol = "sidewalk_both")

# Process before join -------------------------------------------------------------------------

osm_selected <- osm_raw %>% 
  # Select only relevant columns
  select(osm_id, 
         lanes, 
         parking_left, parking_right, parking_both, 
         sidewalk, sidewalk_left, sidewalk_right, sidewalk_both) %>% 
  # Include only segments with lane count
  # filter: removed 157,950 rows (90%), 17,541 rows remaining
  filter(!is.na(lanes)) %>% 
  # Mask out to Philadelphia
  st_filter(phila_mask, .predicate = st_within)

osm_clean <- osm_selected %>% 
  # Combined parking variable
  mutate(parking_lanes =
           case_when(parking_both %in% c("lane", "street_side", "yes") ~ 
                       "Both sides",
                     parking_left %in% c("lane", "street_side", "yes") &
                       parking_right %in% c("lane", "street_side", "yes") ~
                       "Both sides",
                     parking_left %in% c("lane", "street_side", "yes") ~
                       "One side",
                     parking_right %in% c("lane", "street_side", "yes") ~
                       "One side",
                     parking_left == "no" | parking_right == "no" | parking_both == "no" ~
                       "None",
                     is.na(parking_left) & is.na(parking_right) & is.na(parking_both) ~
                       NA,
                     .default = "CHECK")) %>% 
  # Sidewalk absence variable
  mutate(sidewalk_status =
           case_when(sidewalk_both %in% c("both", "separate") ~
                       "Both sides",
                     sidewalk %in% c("both", "separate") ~
                       "Both sides",
                     sidewalk %in% c("left", "right") ~
                       "One side",
                     sidewalk == "no" ~
                       "None",
                     sidewalk_left == "separate" & sidewalk_right == "yes" ~ 
                       "Both sides",
                     sidewalk_right == "separate" & sidewalk_left == "yes" ~ 
                       "Both sides",
                     sidewalk_left %in% c("yes", "separate") ~
                       "One side",
                     sidewalk_right %in% c("yes", "separate") ~
                       "One side",
                     sidewalk == "no" | sidewalk_both == "no" | sidewalk_left == "no" | sidewalk_right == "no" ~
                       "None",
                     is.na(sidewalk) & is.na(sidewalk_both) & is.na(sidewalk_left) & is.na(sidewalk_right) ~
                       NA,
                     .default = "CHECK")) %>% 
  # Remove segments with anomalous lane counts
  mutate(lanes = as.numeric(lanes)) %>% 
  # filter: removed 12 rows (<1%), 11,635 rows remaining
  filter(lanes <= 7) %>% 
  select(osm_id, lanes, parking_lanes, sidewalk_status)

# osm_clean %>% tabyl(parking_lanes)
 # parking_lanesgeometry     n    percent valid_percent
            # Both sides   550 0.04727116     0.5022831
            #       None   165 0.01418135     0.1506849
            #   One side   380 0.03266008     0.3470320
            #       <NA> 10540 0.90588741            NA

# mapview(osm_clean %>% filter(!is.na(parking_lanes)), zcol = "parking_lanes")

# osm_clean %>% st_drop_geometry() %>% tabyl(sidewalk_status)
 # sidewalk_status    n   percent valid_percent
      # Both sides 3726 0.32024065     0.5671233
      #       None  826 0.07099269     0.1257230
      #   One side 2018 0.17344220     0.3071537
      #       <NA> 5065 0.43532445            NA

# mapview(osm_clean %>% filter(!is.na(sidewalk_status)), zcol = "sidewalk_status")

# osm_clean %>% st_drop_geometry() %>% tabyl(lanes)

# lanes    n      percent
 # lanes    n     percent
 #     1 2274 0.195444779
 #     2 5362 0.460850881
 #     3 2058 0.176880103
 #     4 1020 0.087666523
 #     5  679 0.058358401
 #     6  160 0.013751612
 #     7   82 0.007047701

# mapview(osm_clean %>% filter(!is.na(lanes)), zcol = "lanes")

# Prepare join --------------------------------------------------------------------------------

# Based on starting and ending point of segment, get orientation of segment in degrees  
get_orientation <- function(start, end) {
  # Calculate angle (in radians, -pi to pi)
  # 0 is East, pi/2 is North
  angle <- atan2(end[2] - start[2], end[1] - start[1])
  
  # Convert to degrees (0-360)
  angle_deg <- (angle * 180 / pi) %% 360 %>% 
    round(-1) %>% 
    if_else(. >= 180, . - 180, .)
}

osm_ready_to_join <- osm_clean %>%
  select(osm_id) %>% 
  mutate(
    coords = lapply(geometry, st_coordinates),
    start_pt = lapply(coords, function(x) x[1, 1:2]),
    end_pt = lapply(coords, function(x) x[nrow(x), 1:2]),
    orientation_osm = mapply(get_orientation, start_pt, end_pt)
  ) %>% 
  select(osm_id, orientation_osm)

# mapview(osm_ready_to_join, zcol = "orientation_osm")

streets_ready_to_join <- streets_raw %>%
  select(seg_id) %>% 
  mutate(
    coords = lapply(geometry, st_coordinates),
    start_pt = lapply(coords, function(x) x[1, 1:2]),
    end_pt = lapply(coords, function(x) x[nrow(x), 1:2]),
    orientation_streets = mapply(get_orientation, start_pt, end_pt)
  ) %>% 
  select(seg_id, orientation_streets)

# mapview(streets_ready_to_join, zcol = "orientation_streets")

# Join ----------------------------------------------------------------------------------------

# 116.73 sec elapsed
tictoc::tic()
# Join osm network to phila network
joined_raw <- streets_ready_to_join %>% 
  # Bring in any intersecting/close to intersecting OSM segments
  st_join(osm_ready_to_join, 
          join = st_is_within_distance, 
          dist = 20) %>% 
  st_drop_geometry()
tictoc::toc()

joined_processed <- joined_raw %>% 
  # Only accept a join if orientation is same
  # filter: removed 51,939 rows (68%), 24,418 rows remaining
  filter(abs(orientation_osm - orientation_streets) <= 15) %>% 
  mutate(hovertext = str_c("<b>OSM:</b> ", osm_id, "<br><b>Centerlines:</b> ", seg_id, "<br>"))
  
# Test the join -----------------------------------------------------------------------------------

# Tip: 'Show in new window' from viewer and use layer selection button on top right to toggle layers

# All
mapview(joined_processed %>% 
          left_join(osm_ready_to_join %>% select(osm_id)) %>% 
          st_as_sf(crs = "EPSG:2272"), 
        color = "darkred", label = "hovertext", layer.name = "OSM matched") +
  mapview(joined_processed %>% 
            left_join(streets_ready_to_join %>% select(seg_id)) %>% 
            st_as_sf(crs = "EPSG:2272"),
          color = "darkblue", label = "hovertext", layer.name = "Centerlines matched") +
  mapview(osm_ready_to_join %>% 
            filter(!osm_id %in% joined_processed$osm_id) %>% 
            select(osm_id), 
          color = "purple", label = "osm_id", layer.name = "OSM not matched") +
  mapview(streets_ready_to_join %>% 
            filter(!seg_id %in% joined_processed$seg_id) %>% 
            select(seg_id), 
          color = "gray", label = "seg_id", layer.name = "Centerlines not matched")

# Sample of centerlines
set.seed(2718)
streets_sample <- streets_ready_to_join %>% 
  slice_sample(n = 50)

# Checks:
# Centerlines not matched not close to OSM matched or not matched
# OSM not matched not close to centerlines matched or not matched
# Centerlines matched overlaps with OSM matched
mapview(joined_processed %>% 
          filter(seg_id %in% streets_sample$seg_id) %>% 
          left_join(osm_ready_to_join %>% select(osm_id)) %>% 
          st_as_sf(crs = "EPSG:2272"), 
        color = "darkred", label = "hovertext", layer.name = "OSM in sample match") +
  mapview(streets_sample %>% 
            filter(seg_id %in% joined_processed$seg_id),
          color = "darkblue", label = "hovertext", layer.name = "Centerlines in sample match") +
  mapview(streets_sample %>% 
            filter(!seg_id %in% joined_processed$seg_id),
          color = "purple", label = "hovertext", layer.name = "Centerlines sample not matched") +
  mapview(osm_ready_to_join %>% 
            filter(!osm_id %in% joined_processed$osm_id) %>% 
            select(osm_id), 
          color = "darkgray", label = "osm_id", layer.name = "All OSM not matched")

# Sample of OSM
set.seed(2718)
osm_sample <- osm_ready_to_join %>% 
  slice_sample(n = 50)

# Checks:
# Centerlines not matched not close to OSM matched or not matched
# OSM not matched not close to centerlines matched or not matched
# Centerlines matched overlaps with OSM matched
mapview(joined_processed %>% 
          filter(osm_id %in% osm_sample$osm_id) %>% 
          left_join(streets_ready_to_join %>% select(seg_id)) %>% 
          st_as_sf(crs = "EPSG:2272"), 
        color = "darkblue", label = "hovertext", layer.name = "Centerlines in sample match") +
  mapview(osm_sample %>% 
            filter(osm_id %in% joined_processed$osm_id),
          color = "darkred", label = "hovertext", layer.name = "OSM in sample match") +
  mapview(osm_sample %>% 
            filter(!osm_id %in% joined_processed$osm_id),
          color = "purple", label = "hovertext", layer.name = "OSM sample not matched") +
  mapview(streets_ready_to_join %>% 
            filter(!seg_id %in% joined_processed$seg_id) %>% 
            select(seg_id), 
          color = "darkgray", label = "hovertext", layer.name = "All centerlines not matched")

# Remaining issues ----------------------------------------------------------------------------

# Some wide roadways (e.g., parkside, girard, market, parkway, 38th, grant ave) are mapped as
# two roadways side by side in OSM but not in centerlines.
# This may cause issues with some roads overmatching or undermatching.

# The OSM attributes (lane count, parking, sidewalks) have to be normalized at the scale of
# the centerlines data. If a centerlines segment has multiple OSM segments, need to figure
# out how to aggregate/summarize OSM data.




