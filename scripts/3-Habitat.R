### Cleaned Locs - generate random points ====

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'spatsoc', 
          'lubridate', 'raster', 'sp')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS('output/2-clean-all-nn.Rds')

lcFogo <- raster("../nl-landcover/output/fogo_lc.tif")
legend <- fread("../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv")

### Variables ----
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
crs <- CRS("+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

### Plot landcover ----
spplot(lcFogo)

### Extract habitat type at end step
# Easting = x axis = x coord = east to west = longitude
# Northing = y axis = ycoord = north to south = latitude

# extract habitat type
# TODO: why NA
DT[, Value := extract(lcFogo, matrix(c(EASTING, NORTHING), ncol = 2))]

# rename habitat types by merging legend
DT[legend, lc := Landcover, on = 'Value']

# check number of fixes by habitat type 
# note how there are very few broadleaf and mixedwood
DT[, .N, by = lc]

### This next chunk is to calculate the proportion of each habitat type in a given radius
# TODO: why
# Set NAs in lc to class 10
lcFogo[is.na(lcFogo)] <- 10
focals <- lapply(legend$Value, function(val) {
  subs(lcFogo, legend[, .(Value, Value == val)])
})
names(focals) <- legend$Value

# combine habitat types into groupings of your choice 
# using the Value numbers from the legend
openMove <- focals[[1]] + focals[[6]] + focals[[9]]
Forest <- ConiferFogo + MixedWoodFogo + ScrubFogo + BroadleafFogo
Lichen <- LichenFogo## Lichen stays the same

### This step makes new raster layers that are "proportion of habitat within a 100 m 
# buffer that is habitat x". Tends to make analyses more robust and less susceptible
# to problems with autocorrelation.-MPL

# Generate buffer size
# you can change this number, but right now it is a 100m circle around each point
buff <- 100 

openMoveBuff <- focalWeight(openMove, d = buff, type = 'circle')
ForestBuff <- focalWeight(Forest, d = buff, type = 'circle')
LichenBuff <- focalWeight(Lichen, d = buff, type = 'circle')

openMoveBuff100 <- focal(openMove, openMoveBuff, na.rm = TRUE, pad = TRUE, padValue = 0)
ForestBuff100 <- focal(Forest, ForestBuff, na.rm = TRUE, pad = TRUE, padValue = 0)
LichenBuff100 <- focal(Lichen, LichenBuff, na.rm = TRUE, pad = TRUE, padValue = 0)

# Proportion of habitat at each relocation
DT[, propOpenMove := extract(openMoveBuff100, ptsFogo)
DT$propForest <- raster::extract(ForestBuff100,ptsFogo)
DT$propLichen <- raster::extract(LichenBuff100,ptsFogo)

str(DT)

saveRDS(DT, "output/3-clean-all-nn-hab.RDS")


