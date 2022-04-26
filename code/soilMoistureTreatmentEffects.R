# calculate treatment effects compared to control soil moisture

rm(list = ls())

library(tidyverse)
library(lubridate)
library(lme4)
library(emmeans)
library(sheepweather)

# input ---------------------------------------------------- #

load('my_plotting_theme.Rdata')

spotVWC <- usses_spot_sm
decVWC <- usses_decagon

daily_clim <- read_csv('data/temp_data/daily_station_dat_rainfall.csv')  # climate station rainfall

seasons <- read_csv('data/season_table.csv')
quads <- read_csv('data/quad_info.csv')

# output ---------------------------------------------------- #

treatment_effects_model_outfile <- 'output/treatment_sm_model.RDS'
daily_sm_outfile <- 'data/temp_data/daily_sm.csv'

# --------------------------------------------------------------------------------------#

# Steps for treatment effects standardization:
# 1. aggregate soil moisture by plot, Treatment, and date
# 2. then standardize soil moisture measurements by dividing by the daily mean soil moisture in the controls 
# 3. Model with a lmer model 
#       - fixed effects for treatment, season and rainfall 
#       - random effects for plot and date 
#       - use number of independent probe samples as weights 
# 
# 4. Save model above and use to predict treatment effects on the SoilWAT scale 
# 5. Save soilwat time series with predicted treatment effects 


decVWC <- 
  decVWC %>% 
  filter( stat == 'raw', measure == 'VWC', plot != 'X16', !is.na(v)) %>% # drop plot 16 because moisture is anomalous 
  ungroup %>% 
  mutate( unique_position = paste0(plot, '.', position)) %>% 
  mutate( v = v*100 ) %>%                                     # convert to percent 
  mutate( type = 'logger' ) %>% 
  group_by(type, date, PrecipGroup, Treatment, plot) %>% 
  summarise( v = mean(v), weight = n_distinct(unique_position))

spotVWC <- 
  spotVWC %>% 
  mutate( type = 'spot') %>% 
  group_by( type, date, PrecipGroup, Treatment, plot ) %>% 
  summarise( v = mean(VWC, na.rm = T), weight = n() ) 

daily_sm <- 
  bind_rows(decVWC, spotVWC)  %>% 
  group_by( date) %>% 
  mutate( raw = v) %>%  # copy raw data to a new column 
  mutate( v = v/(mean(v[Treatment == 'Control']))) %>% # express soil moisture as proportion of average daily in "control" plots
  left_join(daily_clim, by = 'date') %>% 
  mutate( month = month(date)) %>% 
  left_join(seasons, by= 'month')

m1 <- lmer(v ~ Treatment*season*rainfall + (1|plot) + (1|date), weights = weight, data = daily_sm)
summary(m1)

m2 <- lmer(v ~ Treatment*season*inv_days_since_rain + (1|plot) + (1|date), weights = weight, data = daily_sm)
AIC(m1, m2)

summary(m2)

saveRDS(m2, treatment_effects_model_outfile)

write_csv(daily_sm, daily_sm_outfile)
