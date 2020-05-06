



### Packages ----
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS('output/1-clean-all.Rds')

### Proximity Based Social Networks ----

# Temporal grouping 
locs <- group_times(locs, datetime = 'datetime', threshold = '5 minutes')

group_pts(
  locs,
  threshold = 50,
  splitBy = c('Year'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = c('EASTING', 'NORTHING')
)

### Calculate SRI for each year ----
source("functions/get_sri.R")

nets <- get_sri(locs, id = 'ANIMAL_ID', by = c('Year'))

nets$dyad <- as.factor(paste(nets$ID1, nets$ID2, sep = "_"))

saveRDS(nets, "output/4-sri.RDS")

