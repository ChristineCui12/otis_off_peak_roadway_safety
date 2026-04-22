# Purpose -------------------------------------------------------------------------------------
# Modeling workflow for OTIS off-peak speeding percentage prediction

# Preliminaries -------------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(janitor)
library(scales)
library(tidymodels)
library(boxr)
library(pdp)
library(DALEXtra)
library(sf)
library(patchwork)

# Set up remote data access via Box API
box_auth()
box_raw_data_folder <- 362958311858
box_processed_data_folder <- 362958210990

theme_set(theme_light())

# Read data -----------------------------------------------------------------------------------

# box_ls(362958210990)

# Centerlines data with geometry
centerlines_geometry <- box_read_rds(2139915462983) %>%
  mutate(seg_id = as.character(seg_id)) %>%
  st_transform(crs = "EPSG:2272")

# Getting traffic direction from original speed data
speed_raw <- box_read_rds(2133989776667) %>% 
  select(recordnum, traffic_direction_original = trafdir) %>% 
  distinct()

# Speed data
speed <- box_read_rds(2193174265310) %>% 
  mutate(seg_id = as.character(seg_id)) %>% 
  mutate(speed_measurement_month = as.character(speed_measurement_month)) %>% 
  mutate(speed_measurement_month = str_pad(speed_measurement_month, width = 2, side = "left", pad = "0")) %>% 
  left_join(speed_raw, by = "recordnum") %>% 
  mutate(traffic_direction =
           case_when(traffic_direction_original %in% c("both") ~ "Both",
                     traffic_direction_original %in% c("east", "north", "south", "west") ~ "One way"))
  # left_join(centerlines_geometry %>% 
  #             sf::st_drop_geometry() %>% 
  #             select(seg_id, oneway))

# Hand-checked and added data
data_checked <- box_read_csv(2205423482212) %>% 
  mutate(seg_id = as.character(seg_id)) %>% 
  mutate(wide_shoulder = shoulder_width >= 8) %>% 
  mutate(width_per_traffic_lane = traffic_lanes_width / lanes)

# Crash data
crashes <- box_read(2195155235316) %>% 
  mutate(seg_id = as.character(seg_id)) %>% 
  # Collate into periods and reshape
  mutate(crashes_a = evening + night) %>% 
  mutate(ksi_a = ksi_eve + ksi_night) %>% 
  select(seg_id, 
         crashes_a, 
         crashes_b = midday, 
         crashes_c = pm_peak,
         crashes_d = am_peak,
         ksi_a,
         ksi_b = ksi_midday,
         ksi_c = ksi_pm,
         ksi_d = ksi_am) %>% 
  mutate(ksi_rate_a = ksi_a / crashes_a,
         ksi_rate_b = ksi_b / crashes_b,
         ksi_rate_c = ksi_c / crashes_c,
         ksi_rate_d = ksi_d / crashes_d) %>% 
  pivot_longer(!seg_id, 
               names_pattern = "(.+)_(a|b|c|d)",
               names_to = c("measure", "period"),
               values_to = "value") %>% 
  pivot_wider(names_from = "measure", 
              values_from = "value") %>% 
  mutate(speed_measurement_period =
           case_when(period == "a" ~ "Off-peak (night)",  
                     period == "b" ~ "Off-peak (midday)",
                     period == "c" ~ "Peak (evening)",
                     period == "d" ~ "Peak (morning)")) %>% 
  mutate(speed_measurement_period =
           fct_relevel(speed_measurement_period,
                       "Off-peak (night)", "Off-peak (midday)", "Peak (evening)", "Peak (morning)"))

# Street network data
network_main <- box_read_rds(2151757279199) %>% 
  as_tibble() %>% 
  mutate(bike_lane = 
           case_when(bike_any == TRUE ~ TRUE,
                     bike_any == FALSE ~ FALSE,
                     is.na(bike_any) ~ FALSE)) %>% 
  mutate(width_rms = na_if(surfawidth, 0)) %>% 
  rename(width_dvrpc = NEW_WID, 
         lanes_dvrpc = NEW_LANES,
         arterial_type_dvrpc = TYPOLOGY__,
         divided_roadway = DIV_RDWY,
         road_classification_city = class1_cs, 
         road_classification_fhwa = fhwa_func_desc) %>% 
  # Compile and correct widths
  mutate(width = coalesce(width_rms, width_dvrpc)) %>% 
  mutate(width = case_when(
    seg_id == "340834" ~ 75,
    seg_id == "340836" ~ 72,
    seg_id == "221117" ~ 93,
    seg_id == "421398" ~ 80,
    .default = width
  )) %>% 
  mutate(divided_roadway = case_when(divided_roadway == 1 ~ TRUE,
                                     divided_roadway == 0 ~ FALSE))

network_supplementary <- box_read_rds(2175268420062) %>% 
  mutate(traffic_calming = count_calming > 0)

network_parcels <- box_read_rds(2178038226815)

# Deprecated by manually-checked data

# network_bike <- box_read_rds(2178022998565) %>% 
#   # Clean up bike categories
#   mutate(bike_lane_type_simple =
#            case_when(bike_lane_type %in% c("Advisory Bike Lane", "Sharrow") ~ 
#                        "Sharrow",
#                      bike_lane_type %in% c("Bus/Bike Lane", "Painted Bike Lane") ~ 
#                        "Painted",
#                      bike_lane_type %in% c("None", "Unknown") ~ 
#                        "None",
#                      bike_lane_type %in% c("On-Street Separated Bike Lane", "Raised Separated Bike Lane", "Shared Use Sidepath") ~ 
#                        "Separated",
#                      is.na(bike_lane_type) ~ "None",
#                      .default = "CHECK")) %>% 
#   full_join(data_checked %>% 
#               select(seg_id, year, bike_lane_type_checked = bike_lane_type_simple),
#             by = c("seg_id", "year")) %>% 
#   mutate(bike_lane_status = coalesce(bike_lane_type_checked, bike_lane_type_simple)) %>% 
#   # If type is 'None', exclude here and NA in modeling data will be turned into 'None'.
#   # filter: removed 1,348 rows (6%), 19,628 rows remaining
#   filter(bike_lane_status != "None")

# # OSM data (no longer used now that data have been hand-checked)
# osm_characteristics <- box_read_rds(2174904376082)

# Compile modeling dataset --------------------------------------------------------------------

modeling_data <- speed %>% 
  # Exclude any rows with missing DP
  # filter: removed 180 rows (<1%), 54,132 rows remaining
  filter(!is.na(all_speeding_percent) & !is.na(high_speeding_percent)) %>% 
  mutate(year = year(speed_measurement_date)) %>% 
  select(seg_id, 
         all_speeding_percent,
         high_speeding_percent,
         volume_total,
         speed_measurement_road,
         year, speed_measurement_month, speed_measurement_day_of_week, speed_measurement_period,
         speed_limit, 
         traffic_direction) %>% 
  # Join variable inputs
  left_join(data_checked %>% 
              select(seg_id, 
                     lanes, 
                     divided_roadway, 
                     parking, 
                     sidewalk_status, 
                     bike_lane_status = bike_lane_type_simple,
                     curb_to_curb_width,
                     traffic_lanes_width,
                     width_per_traffic_lane,
                     shoulder_width,
                     wide_shoulder),
            by = "seg_id") %>% 
  # Make sure to treat lanes not as a continuous variable
  mutate(lanes = as.factor(lanes)) %>% 
  left_join(crashes %>%
              select(seg_id, speed_measurement_period, crashes, ksi_rate),
            by = c("seg_id", "speed_measurement_period")) %>%
  left_join(network_main %>% 
              select(seg_id, 
                     length, 
                     arterial_type_dvrpc,
                     road_classification_city,
                     road_classification_fhwa),
            by = "seg_id") %>% 
  left_join(network_supplementary %>% 
              select(seg_id, count_transit, traffic_calming, count_intersection_ctrl),
            by = "seg_id") %>% 
  # left_join(network_bike %>% 
  #             select(seg_id, year, bike_lane_status),
  #           by = c("seg_id", "year")) %>% 
  # Bike lane data are complete, so NA means no lane
  mutate(bike_lane_status = replace_na(bike_lane_status, "None")) %>% 
  # If there is no join, there are 0 properties on that segment
  left_join(network_parcels %>% 
              select(seg_id, parcel_density),
            by = "seg_id") %>% 
  mutate(parcel_density = replace_na(parcel_density, 0)) %>% 
  select(-c(year))

# box_save_rds(modeling_data, file_name = "modeling_data_v9.rds", dir_id = 372762671750)

# Create training/test partition --------------------------------------------------------------

set.seed("2718")
modeling_split <- initial_split(modeling_data, prop = 0.75, strata = all_speeding_percent)

modeling_train <- training(modeling_split)
modeling_test <- testing(modeling_split)

# Specify model -------------------------------------------------------------------------------

# mtry = tune(), min_n = tune()
rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

# Specify recipes -----------------------------------------------------------------------------

target_variable <- 
  c("all_speeding_percent")

minimal_predictors <- 
  c("speed_measurement_period", 
    "lanes",
    "road_classification_city",
    "volume_total")

full_predictors <- 
  c("speed_measurement_period", 
    # "lanes",    # Currently replaced by traffic_lanes_width
    "road_classification_city",
    "volume_total",         
    "speed_measurement_month",
    "speed_measurement_day_of_week",
    "speed_limit",
    "traffic_direction",          
    "divided_roadway",
    "parking",
    "sidewalk_status", 
    "bike_lane_status",
    "parcel_density",
    "count_transit",
    "traffic_calming",
    "count_intersection_ctrl",
    "curb_to_curb_width",
    "traffic_lanes_width",
    "width_per_traffic_lane",
    "wide_shoulder",
    "length")

# Base recipe with target variable
recipe_0 <- recipe(modeling_train) %>% 
  update_role(all_of(target_variable), 
              new_role = "outcome")

# Minimal model (RF): City road classification
recipe_minimal_rf_city <- recipe_0 %>% 
  update_role(all_of(minimal_predictors),
              new_role = "predictor")

# Main model (RF): City road classification
recipe_main_rf_city <- recipe_0 %>% 
  update_role(all_of(full_predictors),
              new_role = "predictor")

# Specify hyperparameter search ---------------------------------------------------------------

# Start course, move finer
# rf_params <- extract_parameter_set_dials(rf_spec) %>%
#   update(
#     mtry = mtry(range = c(2, 15)),
#     min_n = min_n(range = c(5, 50))
#   )

# rf_grid <- grid_space_filling(rf_params, size = 12,  type = "latin_hypercube")

# Collect recipes and models into workflow set ------------------------------------------------

models <-
  workflow_set(preproc = list(minimal_city = recipe_minimal_rf_city, 
                              main_city = recipe_main_rf_city),
               models = list(rf_spec),
               cross = TRUE)

# Create and run resampling folds --------------------------------------------------------------

set.seed("2718")
data_folds <- vfold_cv(modeling_train, v = 10)

metrics <- metric_set(mae, rmse, rsq)
control <- control_resamples(save_pred = TRUE)

tictoc::tic()
# ~25 sec elapsed
model_resamples <- models %>% 
  workflow_map(
    "fit_resamples", 
    # Options to `workflow_map()`: 
    seed = 2718, verbose = TRUE,
    # Options to `fit_resamples()`: 
    resamples = data_folds, 
    metrics = metrics,
    control = control
  )
tictoc::toc()

collect_metrics(model_resamples) 

#   wflow_id                 .config              preproc model       .metric .estimator   mean     n  std_err
#   <chr>                    <chr>                <chr>   <chr>       <chr>   <chr>       <dbl> <int>    <dbl>
# 1 minimal_city_rand_forest Preprocessor1_Model1 recipe  rand_forest mae     standard   0.158     10 0.00140 
# 2 minimal_city_rand_forest Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.214     10 0.00169 
# 3 minimal_city_rand_forest Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.344     10 0.0107  

# 4 main_city_rand_forest    Preprocessor1_Model1 recipe  rand_forest mae     standard   0.0500    10 0.000764
# 5 main_city_rand_forest    Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.0799    10 0.00127 
# 6 main_city_rand_forest    Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.910     10 0.00326 

# Fit to training data ------------------------------------------------------------------------

# Extract the best workflow (using the functions described above)
ranked_workflows <- model_resamples %>% 
  rank_results(rank_metric = "rmse")

best_workflow_id <- ranked_workflows %>% 
  slice(1) |>
  pull(wflow_id)

best_workflow <- models %>% 
  extract_workflow(id = best_workflow_id)

best_fit <- fit(best_workflow, modeling_train)

# # Or manually specify fit of interest
# best_fit <- fit(workflow() %>% 
#                   add_recipe(recipe_main_rf_city) %>% 
#                   add_model(rf_spec),
#                 modeling_train)

# Predict on the model ------------------------------------------------------------------------

predictions <- predict(best_fit, new_data = modeling_data, type = "numeric")

predicted_data <- bind_cols(modeling_data, predictions) %>% 
  rename(predicted_speeding_percent = .pred) %>% 
  relocate(predicted_speeding_percent, .after = seg_id) %>% 
  left_join(centerlines_geometry %>% select(seg_id)) %>% 
  st_as_sf(crs = "EPSG:2272")

# box_save_rds(predicted_data, file_name = "model_predicted_data_draft.rds", dir_id = 372762671750)

# Model explanations --------------------------------------------------------------------------

explainer <- explain_tidymodels(
  model  = best_fit,
  data   = modeling_train %>% select(-all_of(target_variable)),
  y      = modeling_train %>% select(all_of(target_variable)),
  label  = "Random Forest"
)

# Variable importance -------------------------------------------------------------------------

set.seed(2718)
vif <- model_parts(explainer = explainer, 
                   loss_function = loss_root_mean_square,
                   type = "difference",
                   B = 10,
                   N = NULL,
                   variables = full_predictors)

plot(vif)

# Partial dependence plots --------------------------------------------------------------------

pdp_speed_measurement_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "speed_measurement_period",
                N         = NULL) 

pdp_speed_measurement_period_plot <- 
  as_tibble(pdp_speed_measurement_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#ff9500") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by period of day",
       y = "Probability of speeding",
       x = "Period of day")

pdp_speed_measurement_period_plot

# ggsave(plot = pdp_speed_measurement_period_plot, filename = "pdp_hour.svg", width = 6, height = 4)

pdp_lanes <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "traffic_lanes_width",
                N         = NULL) 

pdp_lanes_plot <- 
  as_tibble(pdp_lanes$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by width of roadway for traffic lanes",
       y = "Probability of speeding",
       x = "Width of traffic lanes")

pdp_lanes_density <- ggplot(modeling_train, aes(x = traffic_lanes_width)) +
  geom_density(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_lanes_plot / pdp_lanes_density + plot_layout(heights = c(4, 1))

# ggsave(plot = pdp_lanes_plot, filename = "pdp_lanes.svg", width = 6, height = 4)

pdp_lanes_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "traffic_lanes_width",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_lanes_by_period_plot <- 
  as_tibble(pdp_lanes_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by width of traffic lanes and time of day",
       y = "Probability of speeding",
       x = "Width of traffic lanes",
       color = "Time of day")

pdp_lanes_by_period_plot


#

pdp_width <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "curb_to_curb_width",
                N         = NULL) 

pdp_width_plot <- 
  as_tibble(pdp_width$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by width of roadway",
       y = "Probability of speeding",
       x = "Width (ft)")

pdp_width_density <- ggplot(modeling_train, aes(x = curb_to_curb_width)) +
  geom_density(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_width_plot / pdp_width_density + plot_layout(heights = c(4, 1))

pdp_width_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "curb_to_curb_width",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_width_by_period_plot <- 
  as_tibble(pdp_width_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by curb to curb width and time of day",
       y = "Probability of speeding",
       x = "Curb to curb width",
       color = "Time of day")

pdp_width_by_period_plot

#

pdp_lanewidth <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "width_per_traffic_lane",
                N         = NULL) 

pdp_lanewidth_plot <- 
  as_tibble(pdp_lanewidth$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by width per lane",
       y = "Probability of speeding",
       x = "lane width (ft)")

pdp_lanewidth_density <- ggplot(modeling_train, aes(x = width_per_traffic_lane)) +
  geom_density(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_lanewidth_plot / pdp_lanewidth_density + plot_layout(heights = c(4, 1))

pdp_lanewidth_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "width_per_traffic_lane",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_lanewidth_by_period_plot <- 
  as_tibble(pdp_lanewidth_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by width per lane and time of day",
       y = "Probability of speeding",
       x = "Width per lane",
       color = "Time of day")

pdp_lanewidth_by_period_plot

#

pdp_bikelane <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "bike_lane_status",
                N         = NULL) 

pdp_bikelane_plot <- 
  as_tibble(pdp_bikelane$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by bike lane status",
       y = "Probability of speeding",
       x = "Bike lane status")

pdp_bikelane_density <- ggplot(modeling_train, aes(x = bike_lane_status)) +
  geom_bar(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_bikelane_plot / pdp_bikelane_density + plot_layout(heights = c(4, 1))

pdp_bikelane_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "bike_lane_status",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_bikelane_by_period_plot <- 
  as_tibble(pdp_bikelane_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by bikelane status and time of day",
       y = "Probability of speeding",
       x = "Bikelane status",
       color = "Time of day")

pdp_bikelane_by_period_plot

#

pdp_sidewalk <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "sidewalk_status",
                N         = NULL) 

pdp_sidewalk_plot <- 
  as_tibble(pdp_sidewalk$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by sidewalk status",
       y = "Probability of speeding",
       x = "Sidewalk status")

pdp_sidewalk_density <- ggplot(modeling_train, aes(x = sidewalk_status)) +
  geom_bar(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_sidewalk_plot / pdp_sidewalk_density + plot_layout(heights = c(4, 1))

pdp_sidewalk_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "sidewalk_status",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_sidewalk_by_period_plot <- 
  as_tibble(pdp_sidewalk_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by sidewalk status and time of day",
       y = "Probability of speeding",
       x = "Sidewalk status",
       color = "Time of day")

pdp_sidewalk_by_period_plot

#

pdp_parking <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "parking",
                N         = NULL) 

pdp_parking_plot <- 
  as_tibble(pdp_parking$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by parking status",
       y = "Probability of speeding",
       x = "Parking status")

pdp_parking_density <- ggplot(modeling_train, aes(x = parking)) +
  geom_bar(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_parking_plot / pdp_parking_density + plot_layout(heights = c(4, 1))

pdp_parking_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "parking",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_parking_by_period_plot <- 
  as_tibble(pdp_parking_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by parking status and time of day",
       y = "Probability of speeding",
       x = "Parking status",
       color = "Time of day")

pdp_parking_by_period_plot

#

pdp_traffic_calming <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "traffic_calming",
                N         = NULL) 

pdp_traffic_calming_plot <- 
  as_tibble(pdp_traffic_calming$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by traffic calming status",
       y = "Probability of speeding",
       x = "Traffic calming status")

pdp_traffic_calming_density <- ggplot(modeling_train, aes(x = traffic_calming)) +
  geom_bar(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_traffic_calming_plot / pdp_traffic_calming_density + plot_layout(heights = c(4, 1))

pdp_traffic_calming_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "traffic_calming",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_traffic_calming_by_period_plot <- 
  as_tibble(pdp_traffic_calming_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by traffic calming status and time of day",
       y = "Probability of speeding",
       x = "Traffic calming status",
       color = "Time of day")

pdp_traffic_calming_by_period_plot

###

pdp_length <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "length",
                N         = NULL) 

pdp_length_plot <- 
  as_tibble(pdp_length$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by segment length",
       y = "Probability of speeding",
       x = "Roadway segment length")

pdp_length_density <- ggplot(modeling_train, aes(x = length)) +
  geom_density(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_length_plot / pdp_length_density + plot_layout(heights = c(4, 1))

pdp_length_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "length",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_length_by_period_plot <- 
  as_tibble(pdp_length_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by segment length and time of day",
       y = "Probability of speeding",
       x = "Segment length",
       color = "Time of day")

pdp_length_by_period_plot

#

pdp_road_classification <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "road_classification_city",
                N         = NULL) 

pdp_road_classification_plot <- 
  as_tibble(pdp_road_classification$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by road classification",
       y = "Probability of speeding",
       x = "Road classification")

pdp_road_classification_density <- ggplot(modeling_train, aes(x = road_classification_city)) +
  geom_bar(fill = "#3366CC", alpha = 0.3, color = "#3366CC") +
  scale_y_reverse() +          # flip so density "hangs" below the axis
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    panel.grid    = element_blank()
  ) +
  labs(x = "Distribution of modeling data")

pdp_road_classification_plot / pdp_road_classification_density + plot_layout(heights = c(4, 1))

pdp_road_classification_by_period <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "road_classification_city",
                groups    = "speed_measurement_period",
                N         = NULL)

pdp_road_classification_by_period_plot <- 
  as_tibble(pdp_road_classification_by_period$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by road classification and time of day",
       y = "Probability of speeding",
       x = "Road classification",
       color = "Time of day")

pdp_road_classification_by_period_plot



















# pdp_road_type_by_lanes <- 
#   model_profile(explainer, 
#                 type      = "partial",   
#                 variables = "lanes",
#                 groups = "road_classification_city",
#                 N         = NULL)
# 
# pdp_road_type_by_lanes_plot <- 
#   as_tibble(pdp_road_type_by_lanes$agr_profiles) %>% 
#   mutate(`_groups_` =
#            fct_relevel(`_groups_`,
#                        "Major Arterial",
#                        "Minor Arterial",
#                        "Collector Residential",
#                        "Local Residential")) %>%
#   ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
#   geom_line(linewidth = 1.2, alpha = 0.8) +
#   scale_y_continuous(limits = c(0, NA), 
#                      expand = expansion(mult = c(0, 0.2)),
#                      labels = label_percent()) +
#   # scale_color_manual(values = c("#ff9500", "#ffd000", "#00badb", "#156082")) +
#   labs(title = "Predicted probability of speeding by number of lanes and road type",
#        y = "Probability of speeding",
#        x = "Number of lanes in roadway",
#        color = "Road type")
# 
# pdp_road_type_by_lanes_plot
# 
# # ggsave(plot = pdp_road_type_by_lanes_plot, filename = "pdp_road_type_by_lanes.svg", width = 7, height = 4)
# 
# pdp_traffic_direction_by_lanes <- 
#   model_profile(explainer, 
#                 type      = "partial",   
#                 variables = "lanes",
#                 groups = "traffic_direction",
#                 N         = NULL)
# 
# pdp_traffic_direction_by_lanes_plot <- 
#   as_tibble(pdp_traffic_direction_by_lanes$agr_profiles) %>% 
#   ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
#   geom_line(linewidth = 1.2, alpha = 0.8) +
#   scale_y_continuous(limits = c(0, NA), 
#                      expand = expansion(mult = c(0, 0.2)),
#                      labels = label_percent()) +
#   # scale_color_manual(values = c("#ff9500", "#ffd000", "#00badb", "#156082")) +
#   labs(title = "Predicted probability of speeding by number of lanes and traffic direction",
#        y = "Probability of speeding",
#        x = "Number of lanes in roadway",
#        color = "Traffic direction")
# 
# pdp_traffic_direction_by_lanes_plot
# 
# # ggsave(plot = pdp_traffic_direction_by_lanes_plot, filename = "pdp_traffic_direction_by_lanes.svg", width = 7, height = 4)
# 
# pdp_period_by_lanes <- 
#   model_profile(explainer, 
#                 type      = "partial",   
#                 variables = "speed_measurement_period",
#                 groups    = "lanes",
#                 N         = NULL)
# 
# pdp_period_by_lanes_plot <- 
#   as_tibble(pdp_period_by_lanes$agr_profiles) %>% 
#   ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`, group = `_groups_`)) +
#   geom_line(linewidth = 1.2, alpha = 0.8) +
#   scale_y_continuous(limits = c(0, NA), 
#                      expand = expansion(mult = c(0, 0.2)),
#                      labels = label_percent()) +
#   labs(title = "Predicted probability of speeding by number of lanes and measurement period",
#        y = "Probability of speeding",
#        x = "Number of lanes in roadway",
#        color = "Lane count")
# 
# pdp_period_by_lanes_plot
# plotly::ggplotly(pdp_period_by_lanes_plot)
# 
# # ggsave(plot = pdp_hour_by_lanes_plot, filename = "pdp_hour_by_lanes.svg", width = 7, height = 4)
# 
# # pdp_calming <- 
# #   model_profile(explainer, 
# #                 type      = "partial",   
# #                 variables = "count_calming",
# #                 N         = NULL) 
# # 
# # pdp_calming_plot <- 
# #   as_tibble(pdp_calming$agr_profiles) %>% 
# #   ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
# #   geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
# #   scale_y_continuous(limits = c(0, NA), 
# #                      expand = expansion(mult = c(0, 0.2)),
# #                      labels = label_percent()) +
# #   labs(title = "Predicted probability of speeding by number of traffic calming interventions",
# #        y = "Probability of speeding",
# #        x = "Number of calming interventions")
# # 
# # pdp_calming_plot
# 
# 
# 
# # ggsave(plot = pdp_width_plot, filename = "pdp_width.svg", width = 6, height = 4)
# 
# # pdp_width_per_lane <- 
# #   model_profile(explainer, 
# #                 type      = "partial",   
# #                 variables = "width_per_lane",
# #                 N         = NULL) 
# # 
# # pdp_width_per_lane_plot <- 
# #   as_tibble(pdp_width_per_lane$agr_profiles) %>% 
# #   ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
# #   geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
# #   scale_y_continuous(limits = c(0, NA), 
# #                      expand = expansion(mult = c(0, 0.2)),
# #                      labels = label_percent()) +
# #   labs(title = "Predicted probability of speeding by roadway width per lane",
# #        y = "Probability of speeding",
# #        x = "Width per lane (ft/lane)")
# # 
# # pdp_width_per_lane_plot
# #
# # pdp_width_by_lanes <- 
# #   model_profile(explainer, 
# #                 type      = "partial",   
# #                 variables = "width",
# #                 groups = "lanes",
# #                 N         = NULL)
# # 
# # width_range_by_lanes <- modeling_data %>% 
# #   filter(!is.na(lanes)) %>% 
# #   group_by(lanes) %>% 
# #   summarize(min_width = min(width, na.rm = TRUE), 
# #             max_width = max(width, na.rm = TRUE)) %>% 
# #   mutate(lanes = as.factor(lanes))
# # 
# # pdp_width_by_lanes_plot <- 
# #   as_tibble(pdp_width_by_lanes$agr_profiles) %>% 
# #   mutate(`_groups_` = as.factor(`_groups_`)) %>% 
# #   left_join(width_range_by_lanes, by = c(`_groups_` = "lanes")) %>% 
# #   # Remove width ranges not seen in data
# #   mutate(`_x_` = if_else(`_x_` >= min_width & `_x_` <= max_width, `_x_`, NA)) %>% 
# #   filter(!is.na(`_x_`)) %>% 
# #   ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`)) +
# #   geom_line(linewidth = 1.2, alpha = 0.8) +
# #   scale_y_continuous(limits = c(0, NA), 
# #                      expand = expansion(mult = c(0, 0.2)),
# #                      labels = label_percent()) +
# #   # scale_color_manual(values = c("#ff9500", "#ffd000", "#00badb", "#156082")) +
# #   labs(title = "Predicted probability of speeding by number of lanes and roadway width",
# #        y = "Probability of speeding",
# #        x = "Roadway width (ft)",
# #        color = "Number of")
# # 
# # pdp_width_by_lanes_plot








