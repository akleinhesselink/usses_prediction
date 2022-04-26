# prep cover data 
# Year and quad combinations that are missing for one species 
# but are present for others need to be counted as zeros. 
# Then include all years as NAs for graphing purposes 

rm(list = ls())

species_codes <-   function(x){ 
  # make species codes 
  temp_split <- unlist( lapply( as.list( x), str_split, ' '), recursive = F)
  temp_split <- lapply( temp_split, str_sub, 1, 2 )
  
  toupper( lapply( temp_split, paste0, collapse = ''))
}

sppList=c("ARTR","HECO","POSE","PSSP")
dataDir <- file.path("lib","driversdata/data")

old <- read_csv(file.path(dataDir, 'idaho/allrecords_cover.csv'))
new <- read_csv(file.path(dataDir, 'idaho_modern/allrecords_cover.csv'))
quads <- read_csv(file.path(dataDir, 'idaho_modern/quad_info.csv'))

old$year <- 1900 + old$year

all_records <- 
  bind_rows(old, new) %>% 
  left_join(quads, by = 'quad') %>% 
  mutate( spp = species_codes(species)) %>% 
  dplyr::select( QuadName, Treatment, year, spp, area ) 

all_quads <- 
  all_records %>% 
  filter(! Treatment %in% c('No_shrub', 'No_grass')) %>% 
  distinct(year, QuadName ) 

area <- 
  all_records %>%  
  group_by( QuadName, year, spp ) %>% 
  summarise( area = sum(area, na.rm = T))  %>% 
  filter( spp %in% c(sppList)) %>% 
  ungroup() %>% 
  spread( spp, area ) 

area <- 
  all_quads %>% 
  left_join(area, by = c('QuadName', 'year')) %>% 
  gather( spp, area, sppList ) %>% 
  mutate( area = ifelse(is.na(area), 0, area)) %>% ## fill in zeros 
  mutate( cover = 100*area ) 

# fill in grid of all quad year combinations for figures 
all_years <- expand.grid( QuadName = unique( area$QuadName), 
                          year  = seq(min(area$year) - 1, max(area$year) + 1 , by = 1), 
                          spp = sppList)

cover <- 
  all_years %>% 
  left_join(area, by = c('QuadName', 'year', 'spp')) %>% 
  left_join(quads, by = 'QuadName') %>% 
  dplyr::select(QuadName, Treatment, Group, year, spp, area, cover)


write_csv( cover, 'data/temp_data/all_cover.csv')  
  
