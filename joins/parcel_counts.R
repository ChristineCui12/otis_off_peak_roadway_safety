# Purpose -------------------------------------------------------------------------------------
# Given Philadelphia street centerlines segments, how many properties front each segment?

# Preliminaries -------------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(janitor)
library(scales)
library(boxr)

# Set up remote data access via Box API
box_auth()
box_raw_data_folder <- 362958311858
box_processed_data_folder <- 362958210990

# Read data -----------------------------------------------------------------------------------

# OPA
opa_raw <- box_read_csv(2178021402930)

# Centerlines
streets_raw <- box_read_rds(2139915462983)

# Prepare: OPA --------------------------------------------------------------------------------

opa_clean <- opa_raw %>% 
  # Select and rename objects
  select(# Unique ID
    objectid,
    # Main address variables
    location, unit,
    # Address components (not all accurate)
    house_number, street_direction, street_name, street_designation, 
    # Indicates highest number of address range
    house_extension, 
    # Indicates suffix of address number (e.g., 1234R Market St)
    suffix) %>% 
  # Applying the same series of cleaning steps as in the real estate transfers data
  # No changes are needed with this dataset except for changing road type 'CRESCENT' to 'CRES'
  mutate(street_address_clean = location, .after = location) %>% 
  mutate(street_address_clean = str_remove_all(street_address_clean, "\\\\")) %>% 
  mutate(street_address_clean = str_squish(street_address_clean)) %>% 
  mutate(street_address_clean = na_if(street_address_clean, "")) %>% 
  # Fix street suffix misabbreviations
  mutate(street_address_clean = str_replace(street_address_clean, "\\bLA\\b", "LN")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "\\bWLK\\b", "WALK")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "\\bBLV\\b", "BLVD")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "\\bML\\b", "MALL")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "\\bPK\\b", "PIKE")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "\\bPKY\\b", "PKWY")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "\\bMEW\\b", "MEWS")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "\\bAE\\b", "AVE")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "(?<!\\d )\\bAVENUE\\b", "AVE")) %>% 
  # mutate: changed 80 values (<1%) of 'street_address_clean' (0 new NAs)
  mutate(street_address_clean = str_replace(street_address_clean, "(?<!\\d )\\bCRESCENT\\b", "CRES")) %>% 
  # Spell out words
  mutate(street_address_clean = str_replace(street_address_clean, "\\bCHRIS\\b", "CHRISTOPHER")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "(?<=\\d{1,5} )ST (?!UNIT)", "SAINT ")) %>% 
  mutate(street_address_clean = str_replace(street_address_clean, "(?<=\\d{1,5} )MT (?!UNIT)", "MOUNT ")) %>% 
  # Turn nonsense addresses to NA
  mutate(street_address_clean = if_else(str_length(street_address_clean) == 1, NA, street_address_clean)) 

# Separate out addresses with ranges
opa_simple <- opa_clean %>% 
  distinct(street_address_clean) %>% 
  filter(!str_detect(street_address_clean, "-")) %>% 
  mutate(address_number = as.numeric(str_extract(street_address_clean, "^\\d+"))) %>% 
  mutate(street = str_remove(street_address_clean, "\\d+[A-Z]? (1/2 )?"))

# Expand addresses with ranges
opa_range <- opa_clean %>% 
  distinct(street_address_clean) %>% 
  filter(str_detect(street_address_clean, "-")) %>% 
  mutate(address_remainder = str_remove(street_address_clean, "\\d+[A-Z]?-\\d+ ")) %>% 
  mutate(range_start = str_extract(street_address_clean, "^\\d+")) %>% 
  mutate(range_end = str_extract(street_address_clean, "(?<=\\d{1,10}[A-Z]?-)\\d+")) %>% 
  mutate(range_start_size = str_length(range_start)) %>% 
  mutate(range_end_size = str_length(range_end)) %>% 
  mutate(range_end_complete = 
           if_else(range_start_size == 1,
                   range_end,
                   str_c(str_sub(range_start, 1, range_start_size - 2), range_end))) %>% 
  mutate(range_end_complete = str_remove(range_end_complete, "^0")) %>% 
  mutate(across(c(range_start, range_end_complete), ~as.numeric(.))) %>% 
  mutate(range_diff = range_end_complete - range_start) %>% 
  rowwise() %>% 
  mutate(range_nums = list(seq.int(range_start, range_end_complete, 2))) %>% 
  ungroup() %>% 
  unnest_longer(range_nums) %>% 
  mutate(opa_address_street_alt = str_c(range_nums, " ", address_remainder)) %>% 
  select(street_address_clean, address_number = range_nums, street = address_remainder) %>% 
  mutate(street = str_remove(street, "(1/2 )?"))

opa_to_match <- opa_simple %>% 
  bind_rows(opa_range) %>% 
  mutate(parity = if_else(address_number %% 2 == 0, "Even", "Odd"))

# Prepare: Streets ----------------------------------------------------------------------------

streets_ready <- streets_raw %>% 
  select(seg_id = seg_id,
         street_direction_pre = pre_dir, 
         street_name = st_name, 
         street_abb = st_type, 
         street_direction_post = suf_dir, 
         segment_left_address_from = l_f_add, segment_left_address_to = l_t_add, segment_left_address_block = l_hundred, 
         segment_right_address_from = r_f_add, segment_right_address_to = r_t_add, segment_right_address_block = r_hundred,
         segment_length = length,
         segment_node_from = fnode_, segment_node_to = tnode_) %>% 
  # mutate: changed 24,853 values (60%) of 'street_direction_pre' (0 new NAs)
  #   changed one value (<1%) of 'street_name' (0 new NAs)
  #   changed 100 values (<1%) of 'street_abb' (0 new NAs)
  #   changed 40,992 values (99%) of 'street_direction_post' (0 new NAs)
  mutate(across(where(is.character), ~str_squish(.))) %>% 
  # mutate: changed 24,853 values (60%) of 'street_direction_pre' (24,853 new NAs)
  #   changed 100 values (<1%) of 'street_abb' (100 new NAs)
  #   changed 40,992 values (99%) of 'street_direction_post' (40,992 new NAs)
  mutate(across(where(is.character), ~na_if(., ""))) %>% 
  # Standardize
  # mutate: changed 170 values (<1%) of 'street_name' (0 new NAs)
  mutate(street_name = str_replace(street_name, "^MC ", "MC")) %>% 
  # mutate: changed 2 values (<1%) of 'street_name' (0 new NAs)
  mutate(street_name = str_replace(street_name, "^MUHFELD", "MUHLFELD")) %>% 
  # mutate: changed 2 values (<1%) of 'street_name' (0 new NAs)
  mutate(street_name = str_replace(street_name, "^EAST RIVER", "RIVER")) %>% 
  # mutate: changed 3 values (<1%) of 'street_abb' (3 fewer NAs)
  mutate(street_abb = if_else(street_name == "AYRDALE CRESCENT", "CRES", street_abb)) %>% 
  # mutate: changed 3 values (<1%) of 'street_name' (0 new NAs)
  mutate(street_name = str_replace(street_name, "^AYRDALE CRESCENT", "AYRDALE")) %>% 
  unite(street,
        street_direction_pre, street_name, street_abb, street_direction_post, 
        sep = " ", remove = FALSE, na.rm = TRUE)

streets_to_match <- streets_ready %>% 
  sf::st_drop_geometry() %>% 
  # Get left- and right-side address ranges in different rows
  select(seg_id, street, address_from = segment_left_address_from, address_to = segment_left_address_to) %>% 
  bind_rows(streets_ready %>% 
              select(seg_id, street, address_from = segment_right_address_from, address_to = segment_right_address_to)) %>% 
  # Correct two segments with anomalous to/from address parity mismatch
  filter(!seg_id %in% c(522474, 522473)) %>% 
  add_row(seg_id = 522473, street = "N 33RD ST", address_from = 1601, address_to = 1621) %>% 
  arrange(seg_id) %>% 
  mutate(parity = if_else(address_from %% 2 == 0, "Even", "Odd"))

# Join OPA ------------------------------------------------------------------------------------

opa_streets_join <- opa_to_match %>% 
  inner_join(streets_to_match,
            join_by(street == street,
                    parity == parity,
                    address_number >= address_from,
                    address_number <= address_to)) %>% 
  # distinct: removed 55,204 rows (9%), 548,089 rows remaining
  distinct(street_address_clean, seg_id)

# Aggregate -----------------------------------------------------------------------------------

# Per each centerlines segment, get number of OPA parcels on segment
segment_parcels <- opa_streets_join %>% 
  group_by(seg_id) %>% 
  summarize(parcel_count = n_distinct(street_address_clean)) %>% 
  left_join(streets_ready %>% select(seg_id, segment_length)) %>% 
  # Fix some anomalous counts
  mutate(parcel_count = 
           case_when(seg_id == "980590" ~ 22,
                     seg_id == "980589" ~ 22,
                     .default = parcel_count)) %>% 
  mutate(parcel_density = parcel_count / segment_length) %>% 
  mutate(seg_id = as.character(seg_id))

# test <- streets_raw %>% mutate(seg_id = as.character(seg_id)) %>% inner_join(segment_parcels, by = "seg_id")
# mapview::mapview(test, zcol = "parcel_count")

# Export --------------------------------------------------------------------------------------

# box_save_rds(segment_parcels, file_name = "segment_parcels.rds", dir_id = box_processed_data_folder)











