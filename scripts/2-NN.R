### Cleaned Locs - Calculate NN ====

### Packages ----
libs <- c( 'ggplot2', 'rgdal',  'data.table',
          'spatsoc', 'igraph', 'asnipe')
lapply(libs, require, character.only = TRUE)

#devtools::install_github("ropensci/spatsoc")

### Input raw data ----
DT <- readRDS('output/1-clean-all.Rds')


### Variables ----
projCols <- c('EASTING', 'NORTHING')


### Temporal grouping ----
group_times(DT, datetime = 'datetime', threshold = '5 minutes')

## new column so we can merge NN values back to main dataset
DT$IDYrTime <- as.factor(paste(DT$IDYr, DT$timegroup, sep = "_"))


#####################################################
############# CALCULDATE DISTANCE ##################
####################################################

## Nearest neighbor at end step
edist <- edge_nn(DT = DT, id = 'IDYr', coords = c('EASTING', 'NORTHING'),
                   timegroup = 'timegroup', threshold = 45000, 
                   #fillNA = FALSE,
                   splitBy = c("Year"))

## add the same column as above to merge NN values with main dataset
edist$IDYrTime <- as.factor(paste(edist$ID, edist$timegroup, sep = "_"))

## remove columns already in main dataset
edist[, c("Year", "timegroup", "ID") := NULL]

DT <- merge(DT, edist, by = "IDYrTime")

## Generate spatial groups
DT <- group_pts(
  DT,
  threshold = 50,
  splitBy = c('Year'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = projCols
)

## Export data
saveRDS(DT, 'output/2-clean-all-nn.Rds')


