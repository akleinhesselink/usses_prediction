####################################################################################
#
# run scripts to fetch all demographic data 
# 
####################################################################################

rm(list = ls())
library(tidyverse)

source("code/growth/fetchGrowthData.R")
source("code/survival/fetchSurvData.R")

get_growth_files <- dir('code', pattern = 'get.*growth\\.R', recursive = TRUE, full.names = TRUE)

get_survival_files <- dir('code', pattern = 'get.*survival\\.R', recursive = TRUE, full.names = TRUE)

get_recruitment_files <- dir('code', pattern = 'get.*recruitment\\.R', recursive = TRUE, full.names = TRUE)

lapply( c(get_growth_files, get_survival_files, get_recruitment_files), source) 


