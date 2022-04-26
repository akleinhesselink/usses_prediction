rm(list = ls())

library(tidyverse)
library(lubridate)
library(lme4)
library(climwin)
library(pollen)

# Climate and VWC data  ------------------- # 
quad_info <- read_csv( file = 'data/quad_info.csv')

load( file = 'data/temp_data/daily_weather.rda')
daily_vwc <- readRDS('data/temp_data/daily_swVWC_treatments.RDS') 

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
window_open_max <- 77
window_open_min <- 1
window_exclude_dur <- 3
window_exclude_max <- 20

species <- 'HECO'
for(species in sp_list){ 
  
  intra <- paste0( 'W.' , species )
  dat <- readRDS( paste0 ( 'data/temp_data/', paste0( species, '_size.RDS')))
  dat <- dat[ dat$year < last_year, ]
  
  load( paste0( 'data/temp_data/', paste0( species, '_growth_weekly_ClimWin.rda')))
  
  mnyear <- min(dat$year)
  mxyear <- max(dat$year)
  pid <- unique( dat$pid )
  
  dat <- 
    expand.grid( year = mnyear:mxyear, pid = unique(pid)) %>% 
    left_join(dat, by = c('pid', 'year'))  %>% 
    arrange(pid, year) 
  
  my_dat <- 
    dat %>% 
    dplyr::select(quad, pid, age, year, area, starts_with('W.'))  %>% 
    mutate( W.total = W.ARTR + W.HECO + W.POSE + W.PSSP - eval(parse(text = intra)), 
            W.intra = eval(parse(text = intra)))
  
  growth <- 
    my_dat %>% 
    group_by( pid ) %>% 
    arrange(pid, year) %>% 
    mutate( area = log(area)) %>% 
    mutate( area0 = lag(area)) %>% 
    mutate( growth = area - area0 ) %>% 
    filter( !is.na(growth),  
            is.finite(growth), 
            area > -1.38,                   # filter to larger plants as more reliable indicators of growth
            area0 > -1.38, year > 1925) %>%
    mutate( date = paste0( '15/06/', year)) # date for climwin 
  
  growth <- 
    growth %>% 
    left_join(quad_info, by = 'quad') %>%
    mutate( group_year = paste( Group , year, sep = '_'))
  
  temp_ClimWin_fit <- eval( parse( text = ls()[ str_detect(ls(), species) ]))
  
  m_baseline_old <- temp_ClimWin_fit[[2]]$BestModel # use log VWC as baseline
  growth$VWC_best <- temp_ClimWin_fit[[2]]$BestModelData$`log(climate)`
  
  m_baseline <- lmer( growth ~ 1 + (1|group_year) + VWC_best, data = growth)
  
  growthWin_TEMP <- slidingwin(xvar = list(TMAX = daily_weather$tmax_ts, 
                                           TAVG = daily_weather$TAVG, 
                                           TAVG_0 = daily_weather$TAVG_0, 
                                           TDIF = daily_weather$TDIF),
                               cdate = daily_weather$date_reformat,
                               bdate = growth$date,
                               baseline = m_baseline,
                               cinterval = "week",
                               range = c(window_open_max, window_open_min),
                               exclude = c(window_exclude_dur, window_exclude_max),                              
                               type = "absolute", refday = c(15, 06), 
                               stat = 'mean', 
                               func = c('lin'))
  
  
  growthWin_GDD <- slidingwin(xvar = list(GDD = daily_weather$gdd + 0.00001),
                              cdate = daily_weather$date_reformat,
                              bdate = growth$date,
                              baseline = m_baseline,
                              cinterval = "week",
                              range = c(window_open_max, window_open_min),
                              exclude = c(window_exclude_dur, window_exclude_max),
                              type = "absolute", refday = c(15, 06), 
                              stat = 'sum', 
                              func = c('lin', 'log'))
  
   
  growthWin_merged <- merge_results(growthWin_TEMP, growthWin_GDD)
  
  out_obj_name <- paste(species, 'growth', 'weekly_ClimWin_VWC_cov', sep = '_') # rename climwin results object
  
  assign( 
    out_obj_name, 
    growthWin_merged
  )
  
  save(list = out_obj_name, 
       file = paste0( "data/temp_data/", species, "_growth_weekly_ClimWin_VWC_cov.rda"))
  
  rm(dat, growth)
  rm(list = ls(pattern = species)) 
  rm(list = ls(pattern = "growth")) # remove to clear memory 
  
}
  
