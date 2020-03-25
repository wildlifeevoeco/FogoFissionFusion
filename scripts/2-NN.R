### Cleaned Locs - Calculate NN ====

### Packages ----
libs <- c( 'ggplot2', 'rgdal',  'data.table',
          'spatsoc', 'igraph', 'asnipe')
lapply(libs, require, character.only = TRUE)

#devtools::install_github("ropensci/spatsoc")

### Input raw data ----
DT <- readRDS('output/1-clean-all.Rds')
alloc.col(DT)

### Variables ----
projCols <- c('EASTING', 'NORTHING')


### Temporal grouping ----
group_times(DT, datetime = 'datetime', threshold = '5 minutes')

# new column so we can merge NN values back to main dataset
DT[, IDYrTime := as.factor(paste(IDYr, timegroup, sep = "_"))]



### Nearest neighbor at end step ----
edges <-
  edge_nn(
    DT = DT,
    id = 'IDYr',
    coords = c('EASTING', 'NORTHING'),
    timegroup = 'timegroup',
    threshold = 45000,
    #fillNA = FALSE,
    splitBy = c("Year")
  )

# add the same column as above to merge NN values with main dataset
edges[, IDYrTime := as.factor(paste(ID, timegroup, sep = "_"))]

# remove columns already in main dataset
edges[, c("Year", "timegroup", "ID") := NULL]

DT <- merge(DT, edges, by = "IDYrTime")

### Generate spatial groups ----
group_pts(
  DT,
  threshold = 50,
  splitBy = c('Year'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = projCols
)

### Export data ----
saveRDS(DT, 'output/2-clean-all-nn.Rds')


