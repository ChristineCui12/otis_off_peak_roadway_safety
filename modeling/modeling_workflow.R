# Purpose -------------------------------------------------------------------------------------
# Modeling workflow for OTIS off-peak speeding percentage prediction

# Preliminaries -------------------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(janitor)
library(scales)
library(tidymodels)
library(boxr)

# Set up remote data access via Box API
box_auth()
box_raw_data_folder <- 362958311858
box_processed_data_folder <- 362958210990

# Read data -----------------------------------------------------------------------------------


