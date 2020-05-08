### SRI ====


### Packages ----
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)

### Functions ----
source('functions/get_sri.R')


### Input data ----
locs <- readRDS('output/1-clean-all.Rds')
alloc.col(locs)

### Proximity Based Social Networks ----
# Temporal grouping 
group_times(locs, datetime = 'datetime', threshold = '5 minutes')

group_pts(
  locs,
  threshold = 50,
  splitBy = c('Year'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = c('EASTING', 'NORTHING')
)

# Calculate SRI for each year
nets <- get_sri(locs, id = 'ANIMAL_ID', by = 'Year')[!is.na(sri)]

dyad_id(nets, 'ID1', 'ID2')

### Output ----
saveRDS(nets, 'output/4-sri.Rds')
