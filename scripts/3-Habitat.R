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
lcFogo[is.na(lcFogo)] <- 10

WetlandFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Wetland",1,0)))
BroadleafFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Broadleaf",1,0)))
ConiferFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="ConiferForest",1,0)))
ScrubFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="ConiferScrub",1,0)))
MixedWoodFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="MixedWood",1,0)))
RockFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Rocky",1,0)))
WaterFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Water",1,0)))
LichenFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Lichen",1,0)))
AnthroFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Anthro",1,0)))

## combine habitat types into groupings of your choice 
openMove <- WetlandFogo + RockFogo + WaterFogo + AnthroFogo
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


