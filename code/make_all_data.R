######################################################################################
#
# Make all data for analyses 
#
#####################################################################################
# Require "sheepweather" package.  
# to install use devtools::install_github("git@github.com:akleinhesselink/sheepweather.git")
library(sheepweather)  # this package makes weather and soil moisture data available

# 1. Process the climate data  ------------------------- 
source('code/prepare_data/make_rainfall.R')
#source('code/prepare_data/aggregate_spot_VWC.R')
#source('code/prepare_data/merge_decagon_with_climate_station_data.R')
source('code/soilMoistureTreatmentEffects.R')
source('code/prepare_data/calc_treatment_effects_on_SOILWAT.R')

source('code/aggregate_VWC_data.R')
source('code/make_climate_variables.R') 
source('code/prepare_climate_covariates.R')

# 2. Import and process the demographic data ------------
source('code/get_all_demographic_data.R') # depends on access to driversdata 
source('code/calculate_cover_per_plot.R') # depends on access to driversdata 
source('code/prep_vital_rate_df.R')
source('code/growth/clean_size_data.R')

