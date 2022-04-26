rm(list = ls())

library(tidyverse)
library(lme4)
library(zoo)
library(lubridate)
library(climwin)
library(pollen)

# Climate and VWC data  ------------------- # 
quad_info <- read_csv( file = 'data/quad_info.csv')

daily_weather <- read_csv( file = 'data/temp_data/daily_weather.csv')
daily_vwc <- read_csv('data/temp_data/daily_swVWC_treatments.csv') 

last_year <- 2010 # last year of training data, everything earlier is used 

gdd_low_temp <- 5
gdd_hi_temp <- 30

daily_vwc <- 
  daily_vwc %>% 
  filter( Treatment == 'Control', year(date) < last_year) %>% 
  mutate( date_reformat = paste( str_pad( day( date) , 2, pad = '0'), 
                                 str_pad( month( date ), 2, pad = '0'), 
                                 year(date), sep = '/' ))

daily_weather <- 
  daily_weather %>% 
  filter( year(date) < last_year) %>% 
  rowwise() %>% 
  mutate( date_reformat = paste( str_pad( day( date) , 2, pad = '0'), 
                                 str_pad( month( date ), 2, pad = '0'), 
                                 year(date), sep = '/' )) %>%
  mutate( TAVG = (tmax_ts + tmin_ts)/2 ) %>% 
  mutate( TDIF = tmax_ts - tmin_ts ) %>%   # Daily Temperature differential 
  mutate( TAVG_0 = ifelse( TAVG <= 0, 0, TAVG)) %>%   # Temperatures below zero are set to zero
  mutate( TMAX_K = tmax_ts + 273.15) %>% 
  mutate( gdd = gdd(tmax_ts, tmin_ts, tbase = gdd_low_temp, tbase_max = gdd_hi_temp)) %>% # Growing degree days from pollen
  mutate( PRCP_pos = PRCP + 0.000001)  # ensure PRCP > 0 when log trans.

## ------------------------------------------------- 

sp_list <- c('ARTR', 'HECO', 'POSE', 'PSSP')
iter <- 10
species <- 'ARTR'
window_open_max <- 77
window_open_min <- 1
window_exclude_dur <- 3
window_exclude_max <- 20

for(species in sp_list){ 
  
  intra <- paste0( 'W.' , species )
  dat <- read.csv( paste0 ( 'data/temp_data/', paste0( species, '_survival.csv')))
  
  dat <- 
    dat %>% 
    mutate( pid = paste( quad, trackID, sep = "_")) %>%
    mutate( year = year + 1) %>% # Add one year so that the survival "response" coincides with the current year, not the future
    filter( year < last_year )  
    
  mnyear <- min(dat$year)
  mxyear <- max(dat$year)
  pid <- unique( dat$pid )

  my_dat <- 
    dat %>% 
    dplyr::select(quad, pid, age, year, logarea, survives, starts_with('W.'))  %>% 
    mutate( W.total = W.ARTR + W.HECO + W.POSE + W.PSSP - eval(parse(text = intra)), 
            W.intra = eval(parse(text = intra)))
  
  survival <- 
    my_dat %>% 
    group_by( pid ) %>% 
    arrange(pid, year) %>% 
    filter( !is.na(logarea), 
            !is.na(survives),
            is.finite(logarea), 
            year > 1925) %>%
    mutate( date = paste0( '15/06/', year)) # date for climwin 
  
  survival <- 
    survival %>% 
    left_join(quad_info, by = 'quad') %>%
    mutate( group_year = paste( Group, year , sep = '_'))
  
  m_baseline <- glm( survives ~ 1 + logarea, data = survival, family = 'binomial')
  
  survivesWin_VWC <- slidingwin(xvar = list(VWC = daily_vwc$VWC),
                                cdate = daily_vwc$date_reformat,
                                bdate = survival$date,
                                baseline = m_baseline,
                                cinterval = 'week',
                                range = c(window_open_max, window_open_min),
                                exclude = c(window_exclude_dur, window_exclude_max),
                                type = "absolute", refday = c(15, 06),
                                stat = 'mean', 
                                func = c('lin', 'log'))
  
  
  survivesWin_TEMP <-  slidingwin(xvar = list(TMAX = daily_weather$tmax_ts, 
                                              TAVG = daily_weather$TAVG, 
                                              TAVG_0 = daily_weather$TAVG_0, 
                                              TDIF = daily_weather$TDIF),
                                  cdate = daily_weather$date_reformat,
                                  bdate = survival$date,
                                  baseline = m_baseline,
                                  cinterval = "week",
                                  range = c(window_open_max, window_open_min),
                                  exclude = c(window_exclude_dur, window_exclude_max),                              
                                  type = "absolute", refday = c(15, 06), 
                                  stat = 'mean', 
                                  func = c('lin'))
  
  survivesWin_GDD <- slidingwin(xvar = list(GDD = daily_weather$gdd + 0.00001),
                                cdate = daily_weather$date_reformat,
                                bdate = survival$date,
                                baseline = m_baseline,
                                cinterval = "week",
                                range = c(window_open_max, window_open_min),
                                exclude = c(window_exclude_dur, window_exclude_max),
                                type = "absolute", refday = c(15, 06), 
                                stat = 'sum', 
                                func = c('lin', 'log'))
  
  
  survivesWin_PRCP <- slidingwin(xvar = list(PRCP = daily_weather$PRCP_pos),
                                 cdate = daily_weather$date,
                                 bdate = survival$date,
                                 baseline = m_baseline,
                                 cinterval = "week",
                                 range = c(window_open_max, window_open_min),
                                 exclude = c(window_exclude_dur, window_exclude_max),  
                                 type = "absolute", refday = c(15, 06),
                                 stat = "sum",
                                 func = c("lin", "log"))
  
  
  save(survivesWin_VWC, 
       survivesWin_TEMP, 
       survivesWin_GDD, 
       survivesWin_PRCP, 
       file = paste0( "data/temp_data/", species, "_glm_survival_weekly_ClimWin.rda"))
}
