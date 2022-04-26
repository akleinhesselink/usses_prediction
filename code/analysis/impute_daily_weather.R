rm(list = ls())

library(imputeTS)
library(sheepweather)
library(tidyverse)

daily_weather <- 
  usses_weather %>% 
  spread( ELEMENT, value ) %>%
  mutate( date = lubridate::ymd(date)) %>%
  arrange( date ) %>%
  mutate( tmax_ts = na_interpolation(TMAX), 
          tmin_ts = na_interpolation(TMIN), 
          prcp_ts = ifelse(!is.na(PRCP), PRCP, 0)) # fill missing precip days with 0


write_csv(daily_weather, file = file.path( 'data', 'temp_data', 'daily_weather.csv'))

# Testing 

# test_weather_set <- usses_weather %>% 
#   spread( ELEMENT, value) %>% 
#   mutate( date = lubridate::ymd(date)) %>% 
#   arrange( date ) %>% 
#   mutate( tmax_ts = ts( TMAX ), tmin_ts = ts(TMIN), prcp_ts = ts(PRCP)) 
# 
# 
# TMAX <- test_weather_set$TMAX[ lubridate::year(test_weather_set$date) %in% c(2000:2004) ]
# 
# TMAX_missing <- TMAX_complete <- ts( TMAX )
# test_days <- sample( 1:length(TMAX) , size = 200)
# TMAX_missing[ test_days ] <- NA
# 
# TMAX_spline <- imputeTS::na_interpolation(TMAX_missing, option = 'spline')
# TMAX_stine <- imputeTS::na_interpolation(TMAX_missing, option = 'stine')
# TMAX_lin <- imputeTS::na_interpolation(TMAX_missing, option = 'linear')
# 
# plot( TMAX_spline[test_days], TMAX_complete[test_days])
# cor.test(TMAX_spline[test_days], TMAX_complete[test_days])
# 
# plot( TMAX_stine[test_days], TMAX_complete[test_days])
# cor.test(TMAX_stine[test_days], TMAX_complete[test_days])
# 
# plot( TMAX_lin[test_days], TMAX_complete[test_days])
# cor.test(TMAX_lin[test_days], TMAX_complete[test_days])
# 
# 
