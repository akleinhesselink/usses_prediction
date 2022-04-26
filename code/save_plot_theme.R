rm(list = ls())
library(tidyverse)

# save plotting theme to be used on all plots 

my_theme <- 
  theme_bw () + 
  theme ( panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  strip.background = element_blank())


# black historical 
# olive control-modern 
# orange drought 
# blue  irrigation 


my_colors <- c('black', '#1b9e77', '#d95f02', '#7570b3') 

species_names <- c(bquote( italic('Artemisia')), bquote(italic('Hesperostipa')), bquote(italic('Poa')), bquote(italic('Pseudoroegneria')))

pdf_settings <- c('height' = 5, width = 5 ) 

save(my_colors, species_names, my_theme, file = 'my_plotting_theme.Rdata')
