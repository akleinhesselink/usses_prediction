
# plot size and growth anomolies 
library(ggplot2)
library(stringr) 

rm(list = ls())

df_list <- dir('data/temp_data', '[A-Z]{4}_growth.RDS', full.names = T)

for( i in 1:length(df_list)){ 

  spp <- strsplit(basename(df_list[i]), '_')[[1]][ 1]
  
  df <- readRDS(df_list[i])

  df <- subset(df, Period == 'Historical')

  gs <-  table(df$year, df$Grazing) 
  print(gs)
  
  groups <- table( df$year, df$Group)
  print(groups)
  
  
  df$plantid <- paste ( df$quad , df$trackID , sep = '_' ) 

  print( ggplot( df , aes( x = year, y = logarea.t1, group = plantid, color = Grazing)) + 
    geom_line() + 
    geom_point(size = 0.5) + 
    facet_wrap( ~ Group ) + 
    ggtitle(paste0( spp, ' plant size over time') ) 
  ) 
  df$growth  <- df$logarea.t1 - df$logarea.t0
  print( 
  ggplot( df , aes( x = year, y = growth, group = plantid, color = Grazing)) + 
    geom_line() + 
    facet_wrap(~ Group ) + 
    ggtitle( paste0( spp, ' growth anomoly'))
  ) 
} 

