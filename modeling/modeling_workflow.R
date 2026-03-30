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
  rename(width_dvrpc = NEW_WID) %>% 
  mutate(width = coalesce(width_rms, width_dvrpc))

network_supplementary <- box_read_rds(2175268420062)

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
  left_join(osm_characteristics %>% 
              select(-parking_lanes), 
            by = "seg_id") %>% 
  left_join(crashes %>% 
              select(seg_id, total_crashes, ksi_rate, crash_speed_involvement_rate),
            by = "seg_id") %>% 
  left_join(network_main %>% 
              select(seg_id, 
                     length, width, 
                     road_classification_city = class1_cs, 
                     road_classification_fhwa = fhwa_func_desc, 
                     parking,
                     arterial_type_dvrpc = TYPOLOGY__),
            by = "seg_id") %>% 
  left_join(network_supplementary %>% 
              select(seg_id, count_poles, count_transit, count_calming, count_intersection_ctrl, count_camera),
            by = "seg_id") %>% 
  mutate(year = year(speed_measurement_date)) %>% 
  left_join(network_bike %>% 
              select(seg_id, year, bike_lane_type_simple),
            by = c("seg_id", "year")) %>% 
  # Bike lane data are presumably complete, so NA means no lane
  mutate(bike_lane_type_simple = replace_na(bike_lane_type_simple, "None")) %>% 
  left_join(network_parcels %>% 
              select(seg_id, parcel_density),
            by = "seg_id") %>% 
  select(-year)

# box_save_rds(modeling_data, file_name = "modeling_data_v3.rds", dir_id = 372762671750)

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
              road_classification_fhwa,
              new_role = "predictor")

# Main model (RF)
recipe_main_rf <- recipe_0 %>% 
  update_role(speed_measurement_hour, 
              lanes,
              road_classification_fhwa,
              speed_measurement_road,            # Fixed effect for road name
              speed_measurement_month,
              speed_measurement_day_of_week,
              speed_limit,
              volume_total,                      # Total volume for hour measured
              sidewalk_status, 
              parking,
              bike_lane_type_simple,
              parcel_density,
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

# With city classification
#   wflow_id            .config              preproc model       .metric .estimator   mean     n  std_err
#   <chr>               <chr>                <chr>   <chr>       <chr>   <chr>       <dbl> <int>    <dbl>
# 1 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest mae     standard   0.203     10 0.000616
# 2 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.250     10 0.000762
# 3 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.225     10 0.00401 
# 4 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest mae     standard   0.0620    10 0.000208
# 5 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.0948    10 0.000479
# 6 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.888     10 0.000891

# With FHWA classification + bike lane simplified
#   wflow_id            .config              preproc model       .metric .estimator   mean     n  std_err
#   <chr>               <chr>                <chr>   <chr>       <chr>   <chr>       <dbl> <int>    <dbl>
# 1 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest mae     standard   0.194     10 0.000660
# 2 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.243     10 0.000821
# 3 minimal_rand_forest Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.260     10 0.00422 
# 4 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest mae     standard   0.0605    10 0.000226
# 5 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest rmse    standard   0.0932    10 0.000508
# 6 main_rand_forest    Preprocessor1_Model1 recipe  rand_forest rsq     standard   0.891     10 0.000938

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
  geom_line(size = 1.2, alpha = 0.8, color = "#ff9500") +
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
  geom_line(size = 1.2, alpha = 0.8, color = "#156082") +
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
                groups = "road_type",
                N         = NULL)

pdp_road_type_by_lanes_plot <- 
  as_tibble(pdp_road_type_by_lanes$agr_profiles) %>% 
  mutate(`_groups_` = 
           fct_relevel(`_groups_`, 
                       "Major Arterial", 
                       "Minor Arterial", 
                       "Collector Residential", 
                       "Local Residential")) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, color = `_groups_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  scale_color_manual(values = c("#ff9500", "#ffd000", "#00badb", "#156082")) +
  labs(title = "Predicted probability of speeding by number of lanes and road type",
       y = "Probability of speeding",
       x = "Number of lanes in roadway",
       color = "Road type")

pdp_road_type_by_lanes_plot

# ggsave(plot = pdp_road_type_by_lanes_plot, filename = "pdp_road_type_by_lanes.svg", width = 7, height = 4)

pdp_calming <- 
  model_profile(explainer, 
                type      = "partial",   
                variables = "count_calming",
                N         = NULL) 

pdp_calming_plot <- 
  as_tibble(pdp_calming$agr_profiles) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`, group = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8, color = "#156082") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, 0.2)),
                     labels = label_percent()) +
  labs(title = "Predicted probability of speeding by number of traffic calming interventions",
       y = "Probability of speeding",
       x = "Number of calming interventions")

pdp_calming_plot

# ---------------------------------------------------------------------------------------------

centerlines <- sf::st_read("/Users/chkim/Library/CloudStorage/Box-Box/Phila_OTIS/data/raw/Line_Data/Street_Centerline.geojson") %>% 
  mutate(seg_id = as.character(seg_id))

network_test <- network_main %>% 
  as_tibble() %>% 
  select(seg_id, city_type = class1_cs, fhwa_type = fhwa_func_desc, bike_orig = bike_lane, parking_rms = parking,
         lanes_dvrpc = NEW_LANES, width_dvrpc = NEW_WID, width_rms = width, type_dvrpc = TYPOLOGY__)

centerlines_data <- centerlines %>% 
  filter(seg_id %in% modeling_data$seg_id) %>% 
  left_join(network_test, by = "seg_id") %>% 
  left_join(osm_characteristics %>% select(seg_id, lanes_osm = lanes, parking_osm = parking_lanes), by = "seg_id")

# ---------------------------------------------------------------------------------------------

centerlines_data %>% sf::st_drop_geometry() %>% tabyl(city_type, fhwa_type)
 #             city_type Interstate Local Major Collector Minor Arterial Principal Arterial – Other
 # Collector Residential          1    45              47             27                          1
 #     Local Residential          0    13               1              1                          1
 #        Major Arterial          0     1               1             19                         71
 #        Minor Arterial          1     2              55             93                         50
 #                  <NA>          0     2               3              6                          5

type_principal_arterial <- centerlines_data %>% 
  filter(fhwa_type == "Principal Arterial – Other")

type_minor_arterial <- centerlines_data %>% 
  filter(fhwa_type == "Minor Arterial")

mapview::mapview(type_minor_arterial, zcol = "city_type")

mapview::mapview(centerlines_data, zcol = "city_type") +
  mapview::mapview(centerlines_data, zcol = "fhwa_type")

modeling_data %>% 
  ggplot(aes(x = all_speeding_percent)) +
  geom_density() +
  facet_wrap(~road_classification_fhwa)

modeling_data %>% 
  group_by(road_classification_fhwa) %>% 
  summarize(mean = mean(all_speeding_percent), median = median(all_speeding_percent))
#   road_classification_fhwa    mean median
#   <chr>                      <dbl>  <dbl>
# 1 Interstate                 0.162 0.197 
# 2 Local                      0.126 0.0556
# 3 Major Collector            0.217 0.0562
# 4 Minor Arterial             0.369 0.317 
# 5 Principal Arterial – Other 0.328 0.220 

modeling_data %>% 
  ggplot(aes(x = all_speeding_percent)) +
  geom_density() +
  facet_wrap(~road_classification_city)
  
modeling_data %>% 
  group_by(road_classification_city) %>% 
  summarize(mean = mean(all_speeding_percent), median = median(all_speeding_percent))
#   road_classification_city  mean median
#   <chr>                    <dbl>  <dbl>
# 1 Collector Residential    0.177 0.0759
# 2 Local Residential        0.130 0.0617
# 3 Major Arterial           0.313 0.205 
# 4 Minor Arterial           0.308 0.208 
# 5 NA                       0.302 0.109 

# ---------------------------------------------------------------------------------------------

bike_status_2025 <- network_bike %>% filter(year == 2025)

bike_test_data <- centerlines_data %>% 
  left_join(bike_status_2025, by = "seg_id")

bike_test_data %>% sf::st_drop_geometry() %>% tabyl(bike_lane_type_simple, bike_orig)
 # bike_lane_type_simple FALSE TRUE
 #               Painted     2  107
 #             Separated     0   54
 #               Sharrow     1   38
 #                  <NA>   163   81

mapview::mapview(centerlines_data, zcol = "bike_orig")

# ---------------------------------------------------------------------------------------------

centerlines_data %>% sf::st_drop_geometry() %>% tabyl(parking_rms, parking_osm)
   # parking_rms Both sides None One side NA_
   #         B         19    1        7 176
   #         L          0    0        2  29
   #         N          3    2        4  43
   #         R          0    0        4  26
   #         Z         11    1        2 100
   #      <NA>          0    1        1  14

mapview::mapview(centerlines_data, zcol = "parking_rms")
mapview::mapview(centerlines_data, zcol = "parking_osm")

# ---------------------------------------------------------------------------------------------

mapview::mapview(centerlines_data, zcol = "arterial_type_dvrpc")

modeling_data %>% 
  ggplot(aes(x = all_speeding_percent, color = arterial_type_dvrpc, fill = arterial_type_dvrpc)) +
  geom_density(alpha = 0.1)

modeling_data %>% 
  group_by(arterial_type_dvrpc) %>% 
  summarize(mean = mean(all_speeding_percent), median = median(all_speeding_percent))
#   arterial_type_dvrpc  mean median
#   <chr>               <dbl>  <dbl>
# 1 Narrow Connector    0.393 0.381 
# 2 Narrow Neighborhood 0.301 0.196 
# 3 Wide Connector      0.380 0.329 
# 4 Wide Neighborhood   0.341 0.176 
# 5 NA                  0.158 0.0565

modeling_data %>% tabyl(arterial_type_dvrpc, road_classification_city)
 # arterial_type_dvrpc Collector Residential Local Residential Major Arterial Minor Arterial  NA_
 #    Narrow Connector                   120                 0           2976           4704  432
 # Narrow Neighborhood                   583                48           2760           4629  336
 #      Wide Connector                     0                 0           4728           1176  144
 #   Wide Neighborhood                    96                 0           3290           1176   96
 #                <NA>                 17432              2498            264           5301 1343

modeling_data %>% tabyl(arterial_type_dvrpc, road_classification_fhwa)
 # arterial_type_dvrpc Interstate Local Major Collector Minor Arterial Principal Arterial – Other
 #    Narrow Connector          0     0               0           4536                       3696
 # Narrow Neighborhood          0     0               0           4564                       3792
 #      Wide Connector          0     0               0            888                       5160
 #   Wide Neighborhood          0     0               0           1128                       3530
 #                <NA>         72 17823            7215           1728                          0

# ---------------------------------------------------------------------------------------------

centerlines_data %>% sf::st_drop_geometry() %>% tabyl(lanes_osm, lanes_dvrpc)
 #           lanes_dvrpc                
 # lanes_osm           1  2 3  4 5 6 NA_
 #         1           2 10 0  4 0 0  24
 #         2           1 98 5 13 1 3  73
 #         3           1 28 4  4 0 1   2
 #         4           0  3 1 13 1 0   0
 #         5           0  2 0  4 0 0   0
 #         6           0  0 0  0 0 1   0
 #      <NA>           6 40 1  5 0 0  95

modeling_data %>% distinct(seg_id, lanes) %>% tabyl(lanes)
 # lanes   n     percent valid_percent
 #     1  40 0.089686099   0.133779264
 #     2 194 0.434977578   0.648829431
 #     3  40 0.089686099   0.133779264
 #     4  18 0.040358744   0.060200669
 #     5   6 0.013452915   0.020066890
 #     6   1 0.002242152   0.003344482
 #    NA 147 0.329596413            NA

modeling_data %>% distinct(recordnum, seg_id, lanes) %>% tabyl(lanes)
 # lanes   n     percent valid_percent
 #     1  47 0.085766423   0.125000000
 #     2 222 0.405109489   0.590425532
 #     3  56 0.102189781   0.148936170
 #     4  38 0.069343066   0.101063830
 #     5  12 0.021897810   0.031914894
 #     6   1 0.001824818   0.002659574
 #    NA 172 0.313868613            NA

# ---------------------------------------------------------------------------------------------

width_check <- centerlines_data %>% 
  select(seg_id, contains("width")) %>% 
  mutate(width_diff = width_rms - width_dvrpc)

width_check %>% sf::st_drop_geometry() %>% skimr::skim()  
#   skim_variable n_missing complete_rate  mean    sd  p0 p25 p50 p75 p100 hist 
# 1 width_dvrpc         194        0.565  46.5  16.0   20  34  44  54   99 ▅▇▃▁▁
# 2 width_rms           390        0.126  33.3   8.67  18  26  32  40   70 ▇▆▅▁▁
# 3 width_diff          417        0.0650 -1.55 10.8  -40  -2  -1   0   18 ▁▁▁▇▂

width_check %>% ggplot(aes(x = width_diff)) + geom_density()

# For places with a diff, RMS is more accurate than DVRPC
mapview::mapview(width_check %>% filter(width_diff != 0), zcol = "width_diff")

