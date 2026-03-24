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
# /Box-Box/Phila_OTIS/data/processed/osm_roads_data_all.rds
osm_raw <- box_read_rds(2138013253852) %>% 
  st_transform("EPSG:2272")

# Street segments
# /Box-Box/Phila_OTIS/data/processed/base_network_clean.rds
streets_raw <- box_read_rds(2151757279199) %>% 
  st_transform("EPSG:2272")

# Philadelphia mask
phila_mask <- tigris::counties("PA") %>% 
  filter(NAME == "Philadelphia") %>% 
  st_transform("EPSG:2272") %>% 
  # Extend by 20-ft buffer
  st_buffer(dist = 20)

# Speed data (for coverage)
speed_raw <- box_read_rds(2133989776667)
speed_modeling <- box_read_rds(2171353657698)

speed_segments <- speed_modeling %>% 
  distinct(seg_id) %>% 
  mutate(seg_id = as.character(seg_id)) %>% 
  left_join(streets_raw) %>% 
  st_as_sf(crs = "EPSG:2272")

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
  # Calculate segment length
  mutate(osm_length = as.numeric(st_length(.))) %>% 
  select(osm_id, osm_length, lanes, parking_lanes, sidewalk_status)

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
  
  # Convert to degrees (0-179)
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

streets_ready_to_join <- streets_raw %>%
  select(seg_id) %>% 
  mutate(
    coords = lapply(geometry, st_coordinates),
    start_pt = lapply(coords, function(x) x[1, 1:2]),
    end_pt = lapply(coords, function(x) x[nrow(x), 1:2]),
    orientation_streets = mapply(get_orientation, start_pt, end_pt)
  ) %>% 
  select(seg_id, orientation_streets)

mapview(streets_ready_to_join, zcol = "orientation_streets", color = mapviewPalette("mapviewSpectralColors")) +
  mapview(osm_ready_to_join, zcol = "orientation_osm", color = mapviewPalette("mapviewTopoColors"))

# Test for parallel segments ------------------------------------------------------------------



# Find parallel streets -----------------------------------------------------------------------

# Some wide roadways (e.g., parkside, girard, market, parkway, 38th, grant ave) are mapped as
# two roadways side by side in OSM but not in centerlines.
# This may cause issues with some roads overmatching or undermatching.
# Where are they, for each dataset?

# Find all intersections in a buffer. Then filter for close orientation and minimum distance apart.

find_parallel_segments <- function(data, id_col, orientation_col) {

  id_target <- str_c(id_col, ".target")
  id_comp <- str_c(id_col, ".comp")
  
  orientation_target <- str_c(orientation_col, ".target")
  orientation_comp <- str_c(orientation_col, ".comp")
  
  buffer <- data %>% 
    st_buffer(dist = 60, endCapStyle="FLAT")
  
  joined_intersect <- data %>% 
    st_join(data, suffix = c(".target", ".comp")) %>% 
    st_drop_geometry()
  
  joined_target <- buffer %>% 
    st_join(data, join = st_intersects, left = FALSE, suffix = c(".target", ".comp")) %>% 
    st_drop_geometry() %>% 
    anti_join(joined_intersect) %>% 
    filter(abs(.data[[orientation_target]] - .data[[orientation_comp]]) <= 15)
  
  joined_lines_a <- joined_target %>% 
    left_join(data %>% select(all_of(id_col)),
              join_by({{ id_target }} == {{ id_col}} )) %>% 
    st_as_sf(crs = "EPSG:2272")
  
  joined_lines_b <- joined_target %>% 
    left_join(data %>% select(all_of(id_col)),
              join_by({{ id_comp }} == {{ id_col}} )) %>% 
    st_as_sf(crs = "EPSG:2272")
  
  joined_filtered <- joined_lines_a %>% 
    mutate(distance = as.numeric(st_distance(., joined_lines_b, by_element = TRUE))) %>% 
    filter(distance > 10)
  
}

# Missing some parallel streets when the linestring isn't straight, plus includes complex intersections
osm_parallel <- find_parallel_segments(osm_ready_to_join, "osm_id", "orientation_osm")

mapview(osm_ready_to_join, color = "darkgray") + mapview(osm_parallel)

# Overall ok but finds many local streets that overlap at ends
streets_parallel <- find_parallel_segments(streets_ready_to_join, "seg_id", "orientation_streets")

mapview(streets_ready_to_join, color = "darkgray") + mapview(streets_parallel)

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
  mutate(orientation_diff = abs(orientation_osm - orientation_streets)) %>% 
  # Consider angles near the 0/180 degree wrap-around
  mutate(orientation_diff <- min(orientation_diff, 180 - orientation_diff)) %>% 
  # filter: removed 51,939 rows (68%), 24,418 rows remaining
  filter(orientation_diff <= 15) %>% 
  filter(abs(orientation_osm - orientation_streets) <= 15) %>% 
  # Add any manual matches
  # Girard Ave @ 13th St speed measurement point
  add_row(seg_id = "440059", osm_id = "423965636") %>% 
  # Add explanatory text for mapview
  mutate(hovertext = str_c("<b>OSM:</b> ", osm_id, "<br><b>Centerlines:</b> ", seg_id, "<br>"))

# Merge OSM characteristics -------------------------------------------------------------------

merged_data_initial <- joined_processed %>% 
  select(-hovertext, -contains("orientation")) %>% 
  left_join(osm_clean %>% 
              st_drop_geometry(),
              by = "osm_id") %>% 
  # Manually correct OSM lane counts for segments where the roadway is divided in two in OSM
  # but is physically one surface.
  # mutate: changed 8 values (<1%) of 'lanes' (0 new NAs)
  mutate(lanes =
           # Henry Ave speed measurement point
           case_when(osm_id %in% (joined_processed %>% filter(seg_id == "700001") %>% pull(osm_id)) ~
                       4,
                     .default = lanes))

# # Check which centerlines segments have multiple OSM matches
# merged_data_osm_multiple <- merged_data_initial %>% 
#   get_dupes(seg_id) %>% 
#   group_by(seg_id) %>% 
#   mutate(distinct_lane_counts = n_distinct(lanes, na.rm = TRUE)) %>% 
#   mutate(distinct_parking_lanes = n_distinct(parking_lanes, na.rm = TRUE)) %>% 
#   mutate(distinct_sidewalk_status = n_distinct(sidewalk_status, na.rm = TRUE))

# Per each centerlines segment, take the most common OSM value for each of the
# 3 OSM variables; if there is a tie, take the value from the longest OSM segment
merged_data_lanes <- merged_data_initial %>% 
  group_by(seg_id, lanes) %>% 
  summarize(n = n(), longest_osm = max(osm_length)) %>% 
  ungroup() %>% 
  # If the OSM variable is missing, set count to zero so NA isn't favored over non-missing values
  mutate(n = if_else(is.na(lanes), 0, n)) %>% 
  group_by(seg_id) %>% 
  # slice_max (grouped): removed 1,790 rows (11%), 14,409 rows remaining (removed 0 groups, 13,363 groups remaining)
  slice_max(order_by = n, with_ties = TRUE) %>% 
  # slice_max (grouped): removed 1,046 rows (7%), 13,363 rows remaining (removed 0 groups, 13,363 groups remaining)
  slice_max(order_by = longest_osm, with_ties = FALSE) %>% 
  ungroup()

merged_data_parking <- merged_data_initial %>% 
  group_by(seg_id, parking_lanes) %>% 
  summarize(n = n(), longest_osm = max(osm_length)) %>% 
  ungroup() %>% 
  # If the OSM variable is missing, set count to zero so NA isn't favored over non-missing values
  mutate(n = if_else(is.na(parking_lanes), 0, n)) %>% 
  group_by(seg_id) %>% 
  # slice_max (grouped): removed 561 rows (4%), 13,474 rows remaining (removed 0 groups, 13,363 groups remaining)
  slice_max(order_by = n, with_ties = TRUE) %>% 
  # slice_max (grouped): removed 111 rows (1%), 13,363 rows remaining (removed 0 groups, 13,363 groups remaining)
  slice_max(order_by = longest_osm, with_ties = FALSE) %>% 
  ungroup()
  
merged_data_sidewalk <- merged_data_initial %>% 
  group_by(seg_id, sidewalk_status) %>% 
  summarize(n = n(), longest_osm = max(osm_length)) %>% 
  ungroup() %>% 
  # If the OSM variable is missing, set count to zero so NA isn't favored over non-missing values
  mutate(n = if_else(is.na(sidewalk_status), 0, n)) %>% 
  group_by(seg_id) %>% 
  # slice_max (grouped): removed 1,396 rows (9%), 13,698 rows remaining (removed 0 groups, 13,363 groups remaining)
  slice_max(order_by = n, with_ties = TRUE) %>% 
  # slice_max (grouped): removed 335 rows (2%), 13,363 rows remaining (removed 0 groups, 13,363 groups remaining)
  slice_max(order_by = longest_osm, with_ties = FALSE) %>% 
  ungroup()

# Final 1-to-1 dataset
merged_export <- merged_data_lanes %>% 
  select(seg_id, lanes) %>% 
  left_join(merged_data_parking %>% 
              select(seg_id, parking_lanes),
            by = "seg_id") %>% 
  left_join(merged_data_sidewalk %>% 
              select(seg_id, sidewalk_status),
            by = "seg_id")

# Export --------------------------------------------------------------------------------------

# box_save_rds(merged_export, file_name = "osm_matched_data_v1.rds", dir_id = box_processed_data_folder)

# Notes ----------------------------------------------------------------------------

# Some wide roadways (e.g., parkside, girard, market, parkway, 38th, grant ave) are mapped as
# two roadways side by side in OSM, but are physically continuous pavement.
# Some cases have been manually corrected.

# Some curvy roads do not match even if there is an intersecting road because the start/end
# points of segments are not same between centerlines and OSM, which means that the overall
# orientation of the segments is too divergent between them even if they should match.
# This effects some centerlines segments but not any with speed measurements.

# OSM segments with lane count == 6 are very restricted. Good to be aware of.
mapview(osm_clean %>% filter(lanes %in% c(4, 5, 6)), zcol = "lanes")

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
  mapview(speed_segments %>% 
            filter(seg_id %in% joined_processed$seg_id),
          color = "darkgreen", label = "seg_id", layer.name = "Speed segments matched") +
  mapview(osm_ready_to_join %>% 
            filter(!osm_id %in% joined_processed$osm_id) %>% 
            select(osm_id), 
          color = "purple", label = "osm_id", layer.name = "OSM not matched") +
  mapview(streets_ready_to_join %>% 
            filter(!seg_id %in% joined_processed$seg_id) %>% 
            select(seg_id), 
          color = "gray", label = "seg_id", layer.name = "Centerlines not matched") +
  mapview(speed_segments %>% 
            filter(!seg_id %in% joined_processed$seg_id),
          color = "darkorange", label = "seg_id", layer.name = "Speed segments not matched")

# Sample of centerlines
set.seed(2718)
streets_sample <- streets_ready_to_join %>% 
  slice_sample(n = 50)

# Checks:
# Centerlines not matched: not close to any OSM
# OSM not matched: not close to any centerlines
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
# Centerlines not matched: not close to any OSM
# OSM not matched: not close to any centerlines
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

# All joins which are 1 OSM -> multiple centerlines
osm_join_dupes <- joined_processed %>% 
  get_dupes(osm_id)

mapview(osm_join_dupes %>% 
          left_join(osm_ready_to_join %>% select(osm_id)) %>% 
          st_as_sf(crs = "EPSG:2272"), 
        label = "hovertext", 
        layer.name = "OSM",
        zcol = "osm_id",
        color = mapviewPalette("mapviewTopoColors")) +
  mapview(osm_join_dupes %>% 
            left_join(streets_ready_to_join %>% select(seg_id)) %>% 
            st_as_sf(crs = "EPSG:2272"), 
          label = "hovertext", 
          layer.name = "Centerlines",
          zcol = "seg_id",
          color = mapviewPalette("mapviewSpectralColors"))

# All joins which are 1 centerlines -> multiple OSM
streets_join_dupes <- joined_processed %>% 
  get_dupes(seg_id)

mapview(streets_join_dupes %>% 
          left_join(streets_ready_to_join %>% select(seg_id)) %>% 
          st_as_sf(crs = "EPSG:2272"), 
        label = "hovertext", 
        layer.name = "Centerlines",
        zcol = "seg_id",
        color = mapviewPalette("mapviewTopoColors")) +
  mapview(streets_join_dupes %>% 
            left_join(osm_ready_to_join %>% select(osm_id)) %>% 
            st_as_sf(crs = "EPSG:2272"), 
          label = "hovertext", 
          layer.name = "OSM",
          zcol = "osm_id",
          color = mapviewPalette("mapviewSpectralColors")) 

# All joins which are 1 centerlines -> 1 OSM
join_no_dupes <- joined_processed %>% 
  # filter: removed 23,845 rows (98%), 573 rows remaining
  filter(!seg_id %in% streets_join_dupes$seg_id &
           !osm_id %in% osm_join_dupes$osm_id)

mapview(join_no_dupes %>% 
          left_join(streets_ready_to_join %>% select(seg_id)) %>% 
          st_as_sf(crs = "EPSG:2272"), 
        label = "hovertext", 
        layer.name = "Centerlines",
        zcol = "seg_id",
        color = mapviewPalette("mapviewTopoColors")) +
  mapview(join_no_dupes %>% 
            left_join(osm_ready_to_join %>% select(osm_id)) %>% 
            st_as_sf(crs = "EPSG:2272"), 
          label = "hovertext", 
          layer.name = "OSM",
          zcol = "osm_id",
          color = mapviewPalette("mapviewSpectralColors")) 






