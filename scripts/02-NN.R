### Calculate NN ====

### Packages ----
libs <- c( 'ggplot2', 'rgdal',  'data.table',
          'spatsoc', 'igraph', 'asnipe')
lapply(libs, require, character.only = TRUE)


### Input raw data ----
DT <- readRDS('output/1-clean-all.Rds')
alloc.col(DT)

### Variables ----
projCols <- c('EASTING', 'NORTHING')


### Temporal grouping ----
group_times(DT, datetime = 'datetime', threshold = '5 minutes')


### Nearest neighbor at end step ----
# TODO: set max distance between NN, NN to NA
edges <-
  edge_nn(
    DT = DT,
    id = 'ANIMAL_ID',
    coords = c('EASTING', 'NORTHING'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    splitBy = c('Year')
  )


out <- merge(
  DT,
  edges,
  by.x = c('ANIMAL_ID', 'timegroup', 'Year'),
  by.y = c('ID', 'timegroup', 'Year')
)

### Generate spatial groups ----
# TODO: why generate these again in 4-SRI
group_pts(
  out,
  threshold = 50,
  splitBy = c('Year'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = projCols
)


### Export data ----
saveRDS(out, 'output/2-clean-all-nn.Rds')


