
## Cleaned Locs - Calculate NN ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data w/ rdm points
# Outputs: Cleaned collar data + NNID + NNdist 

### Packages ----
libs <- c( 'ggplot2', 'rgdal',  'data.table',
          'spatsoc', 'igraph', 'asnipe')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- fread("input/FogoCaribou.csv")

DT[, datetime := as.POSIXct(paste(idate, itime), format = "%Y-%m-%d %H:%M:%S" )]

## subset data to only winter (Jan 1 to ~March 15)
DT[JDate > 1 & JDate < 75, season := 'winter']
DT <- DT[!is.na(season),]

## create unique IDYr's
DT$IDYr <- as.factor(paste(DT$ANIMAL_ID, DT$Year, sep = "_"))

# Temporal grouping
DT <- group_times(DT, datetime = 'datetime', threshold = '5 minutes')

#####################################################
############# CALCULDATE DISTANCE ##################
####################################################

## Nearest neighbor at end step
edist <- edge_dist(DT = DT, id = 'IDYr', coords = c('NORTHING', 'EASTING'),
                   timegroup = 'timegroup', threshold = 50, returnDist = TRUE, 
                   splitBy = c("Year"))


###### GENERATE NETWORKS FROM RANDOM POINTS ######
DT <- group_pts(
  DT,
  threshold = 50,
  splitBy = c('Year'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = coords
)

ggplot(DT[Year == "2018"]) +
  geom_point(aes(EASTING, NORTHING, color = ANIMAL_ID))



