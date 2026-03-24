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

# Read data -----------------------------------------------------------------------------------

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
  mutate(width = na_if(surfawidth, 0))

network_supplementary <- box_read_rds(2175268420062)

# Compile modeling dataset --------------------------------------------------------------------

modeling_data <- speed %>% 
  # Exclude any rows with missing DP
  # filter: removed 180 rows (<1%), 54,132 rows remaining
  filter(!is.na(all_speeding_percent) & !is.na(high_speeding_percent)) %>% 
  left_join(osm_characteristics %>% 
              select(-parking_lanes), 
            by = "seg_id") %>% 
  left_join(crashes %>% 
              select(seg_id, total_crashes, ksi_rate, crash_speed_involvement_rate),
            by = "seg_id") %>% 
  left_join(network_main %>% 
              select(seg_id, length, width, road_type = class1_cs, bike_lane, parking),
            by = "seg_id") %>% 
  left_join(network_supplementary %>% 
              select(seg_id, count_poles, count_transit, count_calming, count_intersection_ctrl, count_camera),
            by = "seg_id")

# box_save_rds(modeling_data, file_name = "modeling_data_v2.rds", dir_id = 372762671750)

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

# Minimal model (RF)
recipe_minimal_rf <- recipe_0 %>% 
  update_role(speed_measurement_hour, 
              lanes,
              road_type,
              new_role = "predictor")

# Main model (RF)
recipe_main_rf <- recipe_0 %>% 
  update_role(speed_measurement_hour, 
              lanes,
              road_type,
              speed_measurement_road,            # Fixed effect for road name
              speed_measurement_month,
              speed_measurement_day_of_week,
              speed_limit,
              volume_total,                      # Total volume for hour measured
              sidewalk_status, 
              bike_lane,
              parking,
              count_poles,
              count_transit,
              count_calming,
              count_intersection_ctrl,
              count_camera,
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
  workflow_set(preproc = list(minimal = recipe_minimal_rf, main = recipe_main_rf),
               models = list(rf_spec),
               cross = TRUE)

# Create and run resampling folds --------------------------------------------------------------

set.seed("2718")
data_folds <- vfold_cv(modeling_train, v = 10)

metrics <- metric_set(mae, rmse, rsq)
control <- control_resamples(save_pred = TRUE)

tictoc::tic()
# 126.159 sec elapsed
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

#   wflow_id            .config              preproc model       .metric .estimator   mean     n  std_err
#   <chr>               <chr>                <chr>   <chr>       <chr>   <chr>       <dbl> <int>    <dbl>
# 1 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest mae     standard   0.203     10 0.000616
# 2 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.250     10 0.000762
# 3 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.225     10 0.00401 
# 4 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest mae     standard   0.0605    10 0.000209
# 5 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.0930    10 0.000503
# 6 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.892     10 0.000910

# Fit to training data ------------------------------------------------------------------------

# Extract the best workflow (using the functions described above)
ranked_workflows <- model_resamples %>% 
  rank_results(rank_metric = "rmse")

best_workflow_id <- ranked_workflows %>% 
  slice(1) |>
  pull(wflow_id)

best_workflow <- models %>% 
  extract_workflow(id = best_workflow_id)

tictoc::tic()
best_fit <- fit(best_workflow, modeling_train)
tictoc::toc()

# Partial dependence plots --------------------------------------------------------------------

explainer <- explain_tidymodels(
  model  = best_fit,
  data   = modeling_train %>% select(-all_speeding_percent),  # predictors only
  y      = modeling_train$all_speeding_percent,
  label  = "Random Forest"
)

model_profile(
  explainer, 
  type      = "partial",   
  variables = "speed_measurement_hour",
  N         = NULL        
) %>% 
  plot()

model_profile(
  explainer, 
  type      = "partial",   
  variables = "lanes",
  N         = NULL        
) %>% 
  plot()

model_profile(
  explainer, 
  type      = "partial",   
  variables = "lanes",
  groups = "road_type",
  N         = NULL        
) %>% 
  plot()

# ---------------------------------------------------------------------------------------------

# To do:
# variable descriptions
# read in dvrpc typologies and extra variables when demi is done
# write out updated modeling data
# Change road type variable to PennDOT value
# Hyperparameter tuning
# Error by important variable levels, spatial, focusing on use case




