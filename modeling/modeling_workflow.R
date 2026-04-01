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

# Set up remote data access via Box API
box_auth()
box_raw_data_folder <- 362958311858
box_processed_data_folder <- 362958210990

theme_set(theme_light())

# Read data -----------------------------------------------------------------------------------

# box_ls(362958210990)

# Speed data
speed <- box_read_rds(2171353657698) %>% 
  mutate(seg_id = as.character(seg_id)) %>% 
  mutate(speed_measurement_hour = as.character(speed_measurement_hour)) %>% 
  mutate(speed_measurement_hour = str_pad(speed_measurement_hour, width = 2, side = "left", pad = "0")) %>% 
  mutate(speed_measurement_month = as.character(speed_measurement_month)) %>% 
  mutate(speed_measurement_month = str_pad(speed_measurement_month, width = 2, side = "left", pad = "0")) 

# OSM data
osm_characteristics <- box_read_rds(2174904376082)

# Crash data
crashes <- box_read(2172557691734) %>% 
  mutate(seg_id = as.character(seg_id)) %>% 
  mutate(crash_speed_involvement_rate = speeding / total_crashes)

# Street network data
network_main <- box_read_rds(2151757279199) %>% 
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
  mutate(width = coalesce(width_rms, width_dvrpc)) %>% 
  mutate(divided_roadway = case_when(divided_roadway == 1 ~ TRUE,
                                     divided_roadway == 0 ~ FALSE))

network_supplementary <- box_read_rds(2175268420062) %>% 
  mutate(traffic_calming = count_calming > 0)

network_bike <- box_read_rds(2178022998565) %>% 
  # Clean up bike categories
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

network_parcels <- box_read_rds(2178038226815)

# Compile modeling dataset --------------------------------------------------------------------

modeling_data <- speed %>% 
  # Exclude any rows with missing DP
  # filter: removed 180 rows (<1%), 54,132 rows remaining
  filter(!is.na(all_speeding_percent) & !is.na(high_speeding_percent)) %>% 
  # Join variable inputs
  left_join(osm_characteristics %>% 
              select(seg_id, lanes_osm = lanes, sidewalk_status, parking_osm = parking_lanes), 
            by = "seg_id") %>% 
  left_join(crashes %>% 
              select(seg_id, total_crashes, ksi_rate, crash_speed_involvement_rate),
            by = "seg_id") %>% 
  left_join(network_main %>% 
              select(seg_id, 
                     length, width, 
                     lanes_dvrpc, 
                     parking_rms = parking,
                     arterial_type_dvrpc,
                     divided_roadway,
                     road_classification_city,
                     road_classification_fhwa),
            by = "seg_id") %>% 
  left_join(network_supplementary %>% 
              select(seg_id, count_transit, traffic_calming, count_intersection_ctrl),
            by = "seg_id") %>% 
  mutate(year = year(speed_measurement_date)) %>% 
  left_join(network_bike %>% 
              select(seg_id, year, bike_lane_type_simple),
            by = c("seg_id", "year")) %>% 
  # Bike lane data are presumably complete, so NA means no lane
  mutate(bike_lane_type_simple = replace_na(bike_lane_type_simple, "None")) %>% 
  # If there is no join, there are 0 properties on that segment
  left_join(network_parcels %>% 
              select(seg_id, parcel_density),
            by = "seg_id") %>% 
  mutate(parcel_density = replace_na(parcel_density, 0)) %>% 
  # Coalesce lanes variable 
  mutate(lanes = coalesce(lanes_osm, lanes_dvrpc)) %>% 
  # Combine parking variable
  mutate(parking =
           case_when(!is.na(parking_osm) ~ parking_osm,
                     parking_rms == "B" ~ "Both sides",
                     parking_rms %in% c("L", "R") ~ "One side")) %>% 
  # # Road width per lane variable
  # mutate(width_per_lane = width / lanes) %>% 
  select(-c(year, lanes_osm, lanes_dvrpc, parking_rms, parking_osm))

# box_save_rds(modeling_data, file_name = "modeling_data_v5.rds", dir_id = 372762671750)

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

# # Placeholder for linear model
# rf_lm <- linear_reg()

# Specify recipes -----------------------------------------------------------------------------

recipe_0 <- recipe(modeling_train) %>% 
  update_role(all_speeding_percent, new_role = "outcome")

# Minimal model (RF): City road classification
recipe_minimal_rf_city <- recipe_0 %>% 
  update_role(speed_measurement_hour, 
              lanes,
              road_classification_city,
              new_role = "predictor")

# Minimal model (RF): FHWA road classification
recipe_minimal_rf_fhwa <- recipe_0 %>% 
  update_role(speed_measurement_hour, 
              lanes,
              road_classification_fhwa,
              new_role = "predictor")

# Main model (RF): City road classification
recipe_main_rf_city <- recipe_0 %>% 
  update_role(speed_measurement_hour, 
              lanes,
              road_classification_city,
              divided_roadway,
              speed_measurement_road,            # Fixed effect for road name
              speed_measurement_month,
              speed_measurement_day_of_week,
              speed_limit,
              volume_total,                      # Total volume for hour measured
              sidewalk_status, 
              parking,
              bike_lane_type_simple,
              parcel_density,
              count_transit,
              traffic_calming,
              count_intersection_ctrl,
              length,
              width,
              total_crashes,
              ksi_rate,
              new_role = "predictor")

# Main model (RF): FHWA road classification
recipe_main_rf_fhwa <- recipe_0 %>% 
  update_role(speed_measurement_hour, 
              lanes,
              road_classification_fhwa,
              divided_roadway,
              speed_measurement_road,            # Fixed effect for road name
              speed_measurement_month,
              speed_measurement_day_of_week,
              speed_limit,
              volume_total,                      # Total volume for hour measured
              sidewalk_status, 
              parking,
              bike_lane_type_simple,
              parcel_density,
              count_transit,
              traffic_calming,
              count_intersection_ctrl,
              length,
              width,
              total_crashes,
              ksi_rate,
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
                              minimal_fhwa = recipe_minimal_rf_fhwa, 
                              main_city = recipe_main_rf_city,
                              main_fhwa = recipe_main_rf_fhwa),
               models = list(rf_spec),
               cross = TRUE)

# Create and run resampling folds --------------------------------------------------------------

set.seed("2718")
data_folds <- vfold_cv(modeling_train, v = 10)

metrics <- metric_set(mae, rmse, rsq)
control <- control_resamples(save_pred = TRUE)

tictoc::tic()
# 283.811 sec elapsed
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

#    wflow_id                 .config              preproc model       .metric .estimator   mean     n  std_err
#    <chr>                    <chr>                <chr>   <chr>       <chr>   <chr>       <dbl> <int>    <dbl>
#  1 minimal_city_rand_forest Preprocessor1_Model1 recipe  rand_forest mae     standard   0.203     10 0.000697
#  2 minimal_city_rand_forest Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.251     10 0.000859
#  3 minimal_city_rand_forest Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.223     10 0.00394 

#  4 minimal_fhwa_rand_forest Preprocessor1_Model1 recipe  rand_forest mae     standard   0.193     10 0.000695
#  5 minimal_fhwa_rand_forest Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.243     10 0.000852
#  6 minimal_fhwa_rand_forest Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.259     10 0.00405 

#  7 main_city_rand_forest    Preprocessor1_Model1 recipe  rand_forest mae     standard   0.0615    10 0.000222
#  8 main_city_rand_forest    Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.0947    10 0.000502
#  9 main_city_rand_forest    Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.888     10 0.000951

# 10 main_fhwa_rand_forest    Preprocessor1_Model1 recipe  rand_forest mae     standard   0.0613    10 0.000219
# 11 main_fhwa_rand_forest    Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.0944    10 0.000486
# 12 main_fhwa_rand_forest    Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.889     10 0.000944

# Fit to training data ------------------------------------------------------------------------

# Extract the best workflow (using the functions described above)
ranked_workflows <- model_resamples %>% 
  rank_results(rank_metric = "rmse")

best_workflow_id <- ranked_workflows %>% 
  slice(1) |>
  pull(wflow_id)

best_workflow <- models %>% 
  extract_workflow(id = best_workflow_id)

# 13.587 sec elapsed
tictoc::tic()
best_fit <- fit(best_workflow, modeling_train)
tictoc::toc()

# # Or manually specify fit of interest
# best_fit <- fit(workflow() %>% 
#                   add_recipe(recipe_main_rf_city) %>% 
#                   add_model(rf_spec),
#                 modeling_train)

# Partial dependence plots --------------------------------------------------------------------

explainer <- explain_tidymodels(
  model  = best_fit,
  data   = modeling_train %>% select(-all_speeding_percent),  # predictors only
  y      = modeling_train$all_speeding_percent,
  label  = "Random Forest"
)

pdp_speed_measurement_hour <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "speed_measurement_hour",
                N         = NULL) 

pdp_speed_measurement_hour_plot <- 
  as_tibble(pdp_speed_measurement_hour$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#ff9500") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by hour of day",
       y = "Probability of speeding",
       x = "Hour of day (24-hour time)")

pdp_speed_measurement_hour_plot

# ggsave(plot = pdp_speed_measurement_hour_plot, filename = "pdp_hour.svg", width = 6, height = 4)

pdp_lanes <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "lanes",
                N         = NULL) 

pdp_lanes_plot <- 
  as_tibble(pdp_lanes$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by number of lanes",
       y = "Probability of speeding",
       x = "Number of lanes in roadway")

pdp_lanes_plot

# ggsave(plot = pdp_lanes_plot, filename = "pdp_lanes.svg", width = 6, height = 4)

pdp_road_type_by_lanes <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "lanes",
                groups = "road_classification_fhwa",
                N         = NULL)

pdp_road_type_by_lanes_plot <- 
  as_tibble(pdp_road_type_by_lanes$agr_profiles) %>% 
  # mutate(`_groups_` = 
  #          fct_relevel(`_groups_`, 
  #                      "Major Arterial", 
  #                      "Minor Arterial", 
  #                      "Collector Residential", 
  #                      "Local Residential")) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  # scale_color_manual(values = c("#ff9500", "#ffd000", "#00badb", "#156082")) +
  labs(title = "Predicted probability of speeding by number of lanes and road type",
       y = "Probability of speeding",
       x = "Number of lanes in roadway",
       color = "Road type")

pdp_road_type_by_lanes_plot

# ggsave(plot = pdp_road_type_by_lanes_plot, filename = "pdp_road_type_by_lanes.svg", width = 7, height = 4)

# pdp_calming <- 
#   model_profile(explainer, 
#                 type      = "partial",   
#                 variables = "count_calming",
#                 N         = NULL) 
# 
# pdp_calming_plot <- 
#   as_tibble(pdp_calming$agr_profiles) %>% 
#   ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
#   geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
#   scale_y_continuous(limits = c(0, NA), 
#                      expand = expansion(mult = c(0, 0.2)),
#                      labels = label_percent()) +
#   labs(title = "Predicted probability of speeding by number of traffic calming interventions",
#        y = "Probability of speeding",
#        x = "Number of calming interventions")
# 
# pdp_calming_plot

pdp_width <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "width",
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

pdp_width_plot

# ggsave(plot = pdp_width_plot, filename = "pdp_width.svg", width = 6, height = 4)

pdp_width_per_lane <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "width_per_lane",
                N         = NULL) 

pdp_width_per_lane_plot <- 
  as_tibble(pdp_width_per_lane$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by roadway width per lane",
       y = "Probability of speeding",
       x = "Width per lane (ft/lane)")

pdp_width_per_lane_plot

# pdp_width_by_lanes <- 
#   model_profile(explainer, 
#                 type      = "partial",   
#                 variables = "width",
#                 groups = "lanes",
#                 N         = NULL)
# 
# width_range_by_lanes <- modeling_data %>% 
#   filter(!is.na(lanes)) %>% 
#   group_by(lanes) %>% 
#   summarize(min_width = min(width, na.rm = TRUE), 
#             max_width = max(width, na.rm = TRUE)) %>% 
#   mutate(lanes = as.factor(lanes))
# 
# pdp_width_by_lanes_plot <- 
#   as_tibble(pdp_width_by_lanes$agr_profiles) %>% 
#   mutate(`_groups_` = as.factor(`_groups_`)) %>% 
#   left_join(width_range_by_lanes, by = c(`_groups_` = "lanes")) %>% 
#   # Remove width ranges not seen in data
#   mutate(`_x_` = if_else(`_x_` >= min_width & `_x_` <= max_width, `_x_`, NA)) %>% 
#   filter(!is.na(`_x_`)) %>% 
#   ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`)) +
#   geom_line(linewidth = 1.2, alpha = 0.8) +
#   scale_y_continuous(limits = c(0, NA), 
#                      expand = expansion(mult = c(0, 0.2)),
#                      labels = label_percent()) +
#   # scale_color_manual(values = c("#ff9500", "#ffd000", "#00badb", "#156082")) +
#   labs(title = "Predicted probability of speeding by number of lanes and roadway width",
#        y = "Probability of speeding",
#        x = "Roadway width (ft)",
#        color = "Number of")
# 
# pdp_width_by_lanes_plot












