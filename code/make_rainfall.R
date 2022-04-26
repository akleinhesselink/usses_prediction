rm(list = ls())

library( tidyverse )
library(zoo) # for rollapply
library(lubridate)
library(sheepweather)

# input ---------------------------------------------------- #

weather <- usses_weather # use sheepweather package 

# output ---------------------------------------------------- #

rainfall_outfile <- 'data/temp_data/daily_station_dat_rainfall.csv'

# ---------------------------------------------------------------------------------------

weather <- 
  weather %>% 
  filter( date < date('2018-01-01')) # drop most recent year

weather <-
  weather %>%
  spread( ELEMENT, value)

weather <-
  weather %>%
  mutate( TMEAN = ( TMAX + TMIN ) / 2 ) %>%
  select(date, PRCP, TMEAN) %>%
  mutate( rainfall = rollapply(PRCP, 2, sum, fill = 0, na.rm = TRUE, align = 'right') ) %>%
  mutate( rainfall = ifelse( rainfall > 0.0 & TMEAN > 3 & !is.na(rainfall), 'rainy', 'not rainy')) %>%
  mutate( rainfall = ifelse( is.na(rainfall), 'not rainy', rainfall))

# create a factor listing each rainy period, including the day before the rain

weather <-
  weather %>%
  arrange( desc(date) ) %>%
  mutate( prerain = lag( rainfall, 1) ) %>%
  mutate( prerain = ifelse( prerain == 'rainy' & rainfall == 'not rainy', TRUE, FALSE)) %>%
  arrange( date) %>%
  mutate( prcp_event = factor( cumsum ( prerain ) )) %>%
  group_by( prcp_event, prerain) %>%
  mutate( total_rain = cumsum(ifelse(is.na(PRCP), 0, PRCP) ))

weather <-
  weather %>%
  ungroup() %>%
  mutate( year = year(date)) %>%
  group_by( year ) %>%
  arrange( year, date )

# %>% 
#   mutate( ann_cum_PRCP = ifelse(is.na(PRCP), 0, PRCP))  # I don't know what this does 

weather <- 
  weather %>% 
  ungroup()  %>% 
  filter( ! row_number() == max( row_number() ))

weather <- 
  weather %>% 
  group_by(prcp_event) %>% 
  mutate( days_since_rain = row_number()) %>% 
  mutate( inv_days_since_rain = total_rain/days_since_rain )

write_csv(weather, rainfall_outfile )
