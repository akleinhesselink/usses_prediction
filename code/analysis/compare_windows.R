rm(list = ls())

library(climwin)
library(tidyverse)
library(gridExtra)
library(lubridate)

# Compare with competition to without competition 
climWin_growth_result_files <- dir('data/temp_data/', pattern = 'growth_weekly_ClimWin.rda', full.names = T)
climWin_surv_result_files <- dir('data/temp_data/', pattern = 'glmer_weekly_ClimWin.rda', full.names = T)
results_files <- c(climWin_growth_result_files, climWin_surv_result_files)

for( i in results_files){ 
  load(i)
}


# Functions for processing slidingwin results: 
get_calendar_dates <- function( x , foo_year = 1999, ref_date = '06-15') { 
  # x is results combos from sliding win 
  foo_ref_date <- paste( foo_year, ref_date, sep = '-')
  
  x %>% 
    mutate( foo_date_open = ymd(foo_ref_date) - WindowOpen*7,  
            foo_date_close = ymd(foo_ref_date) - WindowClose*7) %>% 
    mutate( month_day_open = paste( month.abb[month(foo_date_open)], day(foo_date_open), sep = '-'), 
            month_day_close = paste( month.abb[month(foo_date_close)], day(foo_date_close), sep = '-'), 
            years_back_open = floor( foo_year - year( foo_date_open )), 
            years_back_close = floor( foo_year - year(foo_date_close))) %>% 
    mutate( Open = paste( month_day_open, years_back_open, 'year back'), 
            Close = paste( month_day_close, years_back_close, 'year back')) %>% 
    mutate( Open = str_replace(Open, '0 year back', 'current year'), 
            Close = str_replace(Close, '0 year back', 'current year')) %>% 
    select( -starts_with('foo'), -starts_with('month'), -starts_with('year'))
}

get_betas <- function(x){ 
  lapply( x, function(x) x$Dataset[1, c('ModelBeta', 'Std.Error', 'ModWeight')]) %>% 
    do.call(what = rbind)
}

# ARTR Window Analysis 

ARTR_weekly_results <- merge_results(ARTR_growth_weekly_ClimWin, ARTR_survival_weekly_ClimWin)

ARTR_best_windows <- get_calendar_dates(ARTR_weekly_results$combos )

ARTR_best_windows <- 
  ARTR_best_windows %>% 
  bind_cols( get_betas(ARTR_weekly_results) )

titles <- 
  ARTR_best_windows %>% 
  select(response, climate, func ) %>% 
  mutate_all(as.character)  %>% 
  rowwise() %>% 
  mutate( title = paste( response, climate, func, sep = ' ')) %>% 
  select( title) %>%
  unlist()

delta_plots <- beta_plots <- list()

for( i in 1:length(titles)){ 
  delta_plots[[i]] <- plotdelta(ARTR_weekly_results[[i]]$Dataset) + ggtitle(titles[i])

  beta_plots[[i]] <- plotbetas(ARTR_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
}

grid.arrange(grobs = delta_plots[ str_detect(titles, 'VWC')])
grid.arrange(grobs = beta_plots[ str_detect(titles, 'VWC')])

grid.arrange(grobs = delta_plots[ c(1:2, 11:12) ])
grid.arrange(grobs = delta_plots[ c(3:6, 13:16) ], nrow = 2)

y <- list()
for( i in 1:nrow(ARTR_best_windows)){ 
  
  x <- ARTR_weekly_results[[i]]$Dataset 
  x$response <- ARTR_best_windows$response[i]
  x$climate <- ARTR_best_windows$climate[i]
  x$func <- ARTR_best_windows$func[i]
  
  y[[i]] <- x
  
}

windows <- do.call( rbind, y )

windows %>% 
  filter( response == 'growth') %>% 
  ggplot(aes( x = WindowClose, y = WindowOpen, fill = deltaAICc ) ) + 
    geom_tile() +
    facet_grid( response ~ climate + func, scales = 'free' ) +
    scale_fill_gradient(low = 'yellow', high = 'blue') + 
    theme( panel.background = element_blank(), panel.grid = element_blank())

windows %>% 
  filter( response == 'survives') %>% 
  ggplot(aes( x = WindowClose, y = WindowOpen, fill = deltaAICc ) ) + 
  geom_tile() +
  facet_grid( response ~ climate + func, scales = 'free' ) +
  scale_fill_gradient(low = 'yellow', high = 'blue') + 
  theme( panel.background = element_blank(), panel.grid = element_blank())


ARTR_best_windows %>%
  group_by( response, climate ) %>% 
  arrange(response, climate, DeltaAICc)

plotweights(ARTR_growth_weekly_ClimWin[[2]]$Dataset, arrow = T) + ggtitle(titles[2])
plotweights(ARTR_growth_weekly_ClimWin[[1]]$Dataset, arrow = T) + ggtitle(titles[1])

plotweights(ARTR_survival_weekly_ClimWin[[1]]$Dataset, arrow = T) + ggtitle(titles[11])
plotweights(ARTR_survival_weekly_ClimWin[[2]]$Dataset, arrow = T) + ggtitle(titles[12])

plotweights(ARTR_survival_weekly_ClimWin[[1]]$Dataset, arrow = T) + ggtitle(titles[11])
plotweights(ARTR_survival_weekly_ClimWin[[2]]$Dataset, arrow = T) + ggtitle(titles[12])

growth_VWC_mod <- ARTR_growth_weekly_ClimWin[[2]]$BestModel
growth_VWC_data <- ARTR_growth_weekly_ClimWin[[2]]$BestModelData

growth_VWC_data <- 
  growth_VWC_data %>%
  rename( growth = yvar, VWC = `log(climate)`)

# HECO -------------------------------------------------------------------------------------
HECO_weekly_results <- merge_results(HECO_growth_weekly_ClimWin, HECO_survival_weekly_ClimWin)
HECO_best_windows <- get_calendar_dates(HECO_weekly_results$combos )

HECO_best_windows <- 
  HECO_best_windows %>% 
  bind_cols( get_betas(HECO_weekly_results) )

titles <- 
  HECO_best_windows %>% 
  select(response, climate, func ) %>% 
  mutate_all(as.character)  %>% 
  rowwise() %>% 
  mutate( title = paste( response, climate, func, sep = ' ')) %>% 
  select( title) %>%
  unlist()

weight_plots <- delta_plots <- beta_plots <- list()

for( i in 1:length(titles)){ 
  delta_plots[[i]] <- plotdelta(HECO_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
  beta_plots[[i]] <- plotbetas(HECO_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
  weight_plots[[i]] <- plotweights(HECO_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
}

grid.arrange(grobs = delta_plots[ str_detect(titles, 'VWC')])
grid.arrange(grobs = beta_plots[ str_detect(titles, 'VWC')])

grid.arrange(grobs = weight_plots[ str_detect(titles, 'VWC')])


# POSE --------------------------------------------------------- # 
POSE_weekly_results <- merge_results(POSE_growth_weekly_ClimWin, POSE_survival_weekly_ClimWin)
POSE_best_windows <- get_calendar_dates(POSE_weekly_results$combos )

POSE_best_windows <- 
  POSE_best_windows %>% 
  bind_cols( get_betas(POSE_weekly_results) )

titles <- 
  POSE_best_windows %>% 
  select(response, climate, func ) %>% 
  mutate_all(as.character)  %>% 
  rowwise() %>% 
  mutate( title = paste( response, climate, func, sep = ' ')) %>% 
  select( title) %>%
  unlist()

delta_plots <- beta_plots <- list()

for( i in 1:length(titles)){ 
  delta_plots[[i]] <- plotdelta(POSE_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
  beta_plots[[i]] <- plotbetas(POSE_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
  weight_plots[[i]] <- plotweights(POSE_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
  
}

plotdelta(POSE_weekly_results[[1]]$Dataset, arrow = T)
plotdelta(POSE_weekly_results[[2]]$Dataset, arrow = T)

grid.arrange(grobs = delta_plots[ str_detect(titles, 'VWC')])
grid.arrange(grobs = beta_plots[ str_detect(titles, 'VWC')])
grid.arrange(grobs = weight_plots[ str_detect(titles, 'VWC')])

POSE_best_windows


# PSSP --------------------------------------------------------- # 
PSSP_weekly_results <- merge_results(PSSP_growth_weekly_ClimWin, PSSP_survival_weekly_ClimWin)
PSSP_best_windows <- get_calendar_dates(PSSP_weekly_results$combos )

plotdelta(PSSP_growth_weekly_ClimWin[[1]]$Dataset)
plotdelta(PSSP_growth_weekly_ClimWin[[2]]$Dataset, arrow = T)
plotweights(PSSP_growth_weekly_ClimWin[[2]]$Dataset)

plotall( PSSP_growth_weekly_ClimWin[[2]]$Dataset)

PSSP_best_windows <- 
  PSSP_best_windows %>% 
  bind_cols( get_betas(PSSP_weekly_results) )

PSSP_best_windows

titles <- 
  PSSP_best_windows %>% 
  select(response, climate, func ) %>% 
  mutate_all(as.character)  %>% 
  rowwise() %>% 
  mutate( title = paste( response, climate, func, sep = ' ')) %>% 
  select( title) %>%
  unlist()

weight_plots <- delta_plots <- beta_plots <- list()

for( i in 1:length(titles)){ 
  delta_plots[[i]] <- plotdelta(PSSP_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
  
  beta_plots[[i]] <- plotbetas(PSSP_weekly_results[[i]]$Dataset, arrow = T) + ggtitle(titles[i])
  weight_plots[[i]] <- plotweights(PSSP_weekly_results[[i]]$Dataset) + ggtitle(titles[i])
  
}

grid.arrange(grobs = delta_plots[ str_detect(titles, 'VWC')])
grid.arrange(grobs = beta_plots[ str_detect(titles, 'VWC')])
grid.arrange(grobs = weight_plots[ str_detect(titles, 'VWC')])

