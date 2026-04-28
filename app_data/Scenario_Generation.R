# Code to generate predictions for the app
# Produces one long table: existing conditions + all feasible scenarios.
# Retains the full modeling_data structure — only scenario_name is added as
# a new column. Geometry columns affected by the scenario are mutated in place.

library(tidyverse)

# ── Load data -----------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

modeling_data <- readRDS("modeling_data_v9.rds") |>
  mutate(lanes = as.integer(as.character(lanes)))

# ── Width standards (feet, minimums only) -------------------------------------
# PennDOT DM-2 §18.5.17 | Philadelphia ROW §1.4.2 | FHWA Road Diet Guide
W_TRANSIT  <- 11   # rightmost lane always transit-standard
W_TRAVEL   <- 10   # all other through lanes
W_BIKE_P   <-  5   # painted bike lane (per side)
W_BIKE_S   <-  6   # separated bike lane (per side)
W_BUFFER   <-  2   # separation buffer between bike lane and travel lane (per side)
W_PARKING  <-  7   # parking lane (per side)
W_WIDE_SHOULDER <- 8  # threshold for wide_shoulder flag (ft)

# Bike and parking widths used both for feasibility check and column recalculation
bike_width <- function(target_bike, bike_sides) {
  case_when(
    target_bike == "Painted"   ~ bike_sides * W_BIKE_P,
    target_bike == "Separated" ~ bike_sides * (W_BIKE_S + W_BUFFER),
    TRUE                       ~ 0
  )
}

parking_width <- function(target_parking) {
  case_when(
    target_parking == "Both sides" ~ 2 * W_PARKING,
    target_parking == "One side"   ~ W_PARKING,
    TRUE                           ~ 0
  )
}

compute_min_width <- function(target_lanes, target_bike, target_parking, bike_sides) {
  travel_w <- W_TRANSIT + (target_lanes - 1) * W_TRAVEL
  travel_w + bike_width(target_bike, bike_sides) + parking_width(target_parking)
}

# ── Scenario catalog -----------------------------------------------------------
# delta_lanes:     integer offset from existing lanes (NA = set to 2)
# target_bike:     proposed bike lane type (NA = keep existing)
# target_parking:  proposed parking configuration (NA = keep existing)
# bike_sides:      1 = one side, 2 = both sides, NA = no bike lane change

scenarios <- tribble(
  ~scenario_name,                              ~delta_lanes,  ~target_bike,  ~target_parking,  ~bike_sides,
  # Existing conditions
  "existing_conditions",                        0L,             NA,            NA,               NA,
  # Bike infrastructure — one side
  "add_painted_bike_lane_one_side",             0L,            "Painted",      NA,               1L,
  "add_separated_bike_lane_one_side",           0L,            "Separated",    NA,               1L,
  "upgrade_painted_to_separated_one_side",      0L,            "Separated",    NA,               1L,
  # Bike infrastructure — both sides (two-way streets only)
  "add_painted_bike_lane_both_sides",           0L,            "Painted",      NA,               2L,
  "add_separated_bike_lane_both_sides",         0L,            "Separated",    NA,               2L,
  "upgrade_painted_to_separated_both_sides",    0L,            "Separated",    NA,               2L,
  # Lane diet — no bike change
  "lane_diet",                                 -1L,             NA,            NA,               NA,
  # Lane diet — one side bike
  "lane_diet_add_painted_one_side",            -1L,            "Painted",      NA,               1L,
  "lane_diet_add_separated_one_side",          -1L,            "Separated",    NA,               1L,
  "lane_diet_upgrade_to_separated_one_side",   -1L,            "Separated",    NA,               1L,
  # Lane diet — both sides bike
  "lane_diet_add_painted_both_sides",          -1L,            "Painted",      NA,               2L,
  "lane_diet_add_separated_both_sides",        -1L,            "Separated",    NA,               2L,
  "lane_diet_upgrade_to_separated_both_sides", -1L,            "Separated",    NA,               2L,
  # Lane diet — parking
  "lane_diet_add_parking",                     -1L,             NA,           "Both sides",      NA,
  # Major road diet (4+ lanes -> 2 through lanes)
  "major_road_diet_to_2_lanes",                 NA_integer_,    NA,            NA,               NA,
  # Major road diet — one side bike
  "major_road_diet_add_painted_one_side",       NA_integer_,   "Painted",      NA,               1L,
  "major_road_diet_add_separated_one_side",     NA_integer_,   "Separated",    NA,               1L,
  # Major road diet — both sides bike
  "major_road_diet_add_painted_both_sides",     NA_integer_,   "Painted",      NA,               2L,
  "major_road_diet_add_separated_both_sides",   NA_integer_,   "Separated",    NA,               2L,
  # Parking changes
  "remove_parking_one_side",                    0L,             NA,            NA,               NA,
  "remove_parking_both_sides",                  0L,             NA,           "None",            NA,
  "add_parking_one_side",                       0L,             NA,           "One side",        NA,
  "add_parking_both_sides",                     0L,             NA,           "Both sides",      NA,
)

# ── Cross all observations x scenarios and apply feasibility ------------------

app_data <- modeling_data |>
  cross_join(scenarios) |>
  mutate(
    # ── Resolve scenario geometry ────────────────────────────────────────────
    target_lanes   = if_else(is.na(delta_lanes), 2L, pmax(1L, lanes + delta_lanes)),
    target_bike    = if_else(is.na(target_bike),    bike_lane_status, target_bike),
    target_parking = case_when(
      scenario_name == "remove_parking_one_side" & parking == "Both sides" ~ "One side",
      scenario_name == "remove_parking_one_side" & parking == "One side"   ~ "None",
      is.na(target_parking)                                                 ~ parking,
      TRUE                                                                  ~ target_parking
    ),
    
    # is_oneway must be defined before eff_bike_sides which depends on it
    is_oneway = lanes == 1 & !divided_roadway,
    
    # eff_bike_sides: explicit scenario sides if specified; otherwise assume
    # existing bike lane occupies 1 side. 0 for None/Sharrow (no width consumed).
    eff_bike_sides = case_when(
      !is.na(bike_sides)                              ~ as.integer(bike_sides),
      bike_lane_status %in% c("None", "Sharrow")      ~ 0L,
      TRUE                                            ~ 1L
    ),
    
    # ── Precondition checks ──────────────────────────────────────────────────
    precond_pass = case_when(
      scenario_name == "existing_conditions"                         ~ TRUE,
      
      scenario_name %in% c("add_painted_bike_lane_one_side",
                           "add_separated_bike_lane_one_side")      ~ bike_lane_status %in% c("None", "Sharrow"),
      scenario_name == "upgrade_painted_to_separated_one_side"       ~ bike_lane_status == "Painted",
      
      scenario_name == "add_painted_bike_lane_both_sides"            ~ !is_oneway & bike_lane_status %in% c("None", "Sharrow"),
      scenario_name == "add_separated_bike_lane_both_sides"          ~ !is_oneway & bike_lane_status %in% c("None", "Sharrow"),
      scenario_name == "upgrade_painted_to_separated_both_sides"     ~ !is_oneway & bike_lane_status == "Painted",
      
      scenario_name == "lane_diet"                                    ~ lanes >= 2,
      scenario_name %in% c("lane_diet_add_painted_one_side",
                           "lane_diet_add_separated_one_side")        ~ lanes >= 2 & bike_lane_status %in% c("None", "Sharrow"),
      scenario_name == "lane_diet_upgrade_to_separated_one_side"       ~ lanes >= 2 & bike_lane_status == "Painted",
      scenario_name %in% c("lane_diet_add_painted_both_sides",
                           "lane_diet_add_separated_both_sides")       ~ lanes >= 2 & !is_oneway & bike_lane_status %in% c("None", "Sharrow"),
      scenario_name == "lane_diet_upgrade_to_separated_both_sides"     ~ lanes >= 2 & !is_oneway & bike_lane_status == "Painted",
      scenario_name == "lane_diet_add_parking"                         ~ lanes >= 2 & parking != "Both sides",
      
      scenario_name == "major_road_diet_to_2_lanes"                   ~ lanes >= 4,
      scenario_name %in% c("major_road_diet_add_painted_one_side",
                           "major_road_diet_add_separated_one_side")   ~ lanes >= 4 & bike_lane_status %in% c("None", "Sharrow"),
      scenario_name %in% c("major_road_diet_add_painted_both_sides",
                           "major_road_diet_add_separated_both_sides")  ~ lanes >= 4 & !is_oneway & bike_lane_status %in% c("None", "Sharrow"),
      
      scenario_name == "remove_parking_one_side"                      ~ parking %in% c("Both sides", "One side"),
      scenario_name == "remove_parking_both_sides"                    ~ parking == "Both sides",
      scenario_name == "add_parking_one_side"                           ~ parking == "None",
      scenario_name == "add_parking_both_sides"                        ~ parking != "Both sides",
      
      TRUE ~ FALSE
    ),
    
    # ── Width feasibility ────────────────────────────────────────────────────
    scenario_min_width = pmap_dbl(
      list(target_lanes, target_bike, target_parking, eff_bike_sides),
      compute_min_width
    ),
    width_pass = case_when(
      scenario_name == "existing_conditions" ~ TRUE,
      TRUE                                   ~ curb_to_curb_width >= scenario_min_width
    ),
    slack_ft = curb_to_curb_width - scenario_min_width
    
  ) |>
  filter(precond_pass, width_pass) |>
  
  # ── Mutate affected columns in place ─────────────────────────────────────
  # Delta-based approach: start from observed traffic_lanes_width and apply
  # changes on top. This correctly preserves existing_conditions (all deltas = 0)
  # and avoids the curb_to_curb reverse-engineering problem (unaccounted space
  # between lane markings, buffers etc. means curb - bike - parking - shoulder
  # does not equal the observed traffic_lanes_width).
  mutate(
    # Save originals before any overwriting
    orig_lanes           = lanes,
    orig_width_per_lane  = width_per_traffic_lane,
    orig_traffic_lanes_w = traffic_lanes_width,
    orig_parking         = parking,
    orig_bike            = bike_lane_status,
    
    # Width consumed by original bike infrastructure.
    # Assumption: existing bike lane occupies 1 side only. No side count in data.
    orig_bike_w = bike_width(
      orig_bike,
      if_else(orig_bike %in% c("None", "Sharrow"), 0L, 1L)
    ),
    
    # Width consumed by scenario bike infrastructure
    new_bike_w  = bike_width(target_bike, eff_bike_sides),
    
    # Width consumed by original and scenario parking
    orig_park_w = parking_width(orig_parking),
    new_park_w  = parking_width(target_parking),
    
    # Direct geometry changes
    lanes            = target_lanes,
    bike_lane_status = target_bike,
    parking          = target_parking,
    
    # Recompute traffic_lanes_width:
    # Two cases depending on whether lane count changes:
    # No lane change: bike/parking delta flows directly to/from traffic lanes.
    #   Removing bike/parking widens lanes; adding narrows them.
    # Lane count changes: freed lane space absorbs new bike/parking first.
    #   Remaining lanes only shrink if additional needs exceed freed space.
    freed_lane_w = pmax(0, (orig_lanes - target_lanes) * orig_width_per_lane),
    traffic_lanes_width = if_else(
      target_lanes == orig_lanes,
      orig_traffic_lanes_w - (new_bike_w - orig_bike_w) - (new_park_w - orig_park_w),
      target_lanes * orig_width_per_lane
      - pmax(0, (new_bike_w - orig_bike_w) + (new_park_w - orig_park_w) - freed_lane_w)
    ),
    
    # Average width per travel lane
    width_per_traffic_lane = traffic_lanes_width / target_lanes,
    
    # wide_shoulder threshold is 8 ft; shoulder_width is unchanged
    wide_shoulder = shoulder_width >= W_WIDE_SHOULDER
    
  ) |>
  # Secondary filter: drop rows where the resulting traffic_lanes_width is below
  # the minimum viable travel width. This catches cases where the delta formula
  # produces a narrow or negative result on streets where observed lane widths
  # are already below standard minimums, even though the curb-width check passed.
  filter(
    scenario_name == "existing_conditions" |
      (
        traffic_lanes_width >= W_TRANSIT + (target_lanes - 1) * W_TRAVEL &
          traffic_lanes_width <= curb_to_curb_width
      )
  ) |>
  
  # Drop scenario construction columns — keep original data structure + scenario_name only
  select(-delta_lanes, -target_bike, -target_parking, -bike_sides,
         -eff_bike_sides, -target_lanes, -is_oneway,
         -orig_lanes, -orig_width_per_lane, -orig_traffic_lanes_w,
         -orig_parking, -orig_bike,
         -orig_bike_w, -new_bike_w, -orig_park_w, -new_park_w,
         -freed_lane_w,
         -precond_pass, -scenario_min_width, -width_pass, -slack_ft)

write_csv(app_data, "model_scenario_predicted_data_draft_v4.csv")