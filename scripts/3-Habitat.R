
## Cleaned Locs - generate random points ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data + NN
# Outputs: 

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'spatsoc', 'amt',
          'tidyverse', 'lubridate', 'raster', 'sp')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS('output/2-clean-all-nn.Rds')

## Variables
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
crs = CRS("+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#### NOTE -- YOU NEED TO SET YOUR OWN DIRECTORY HERE!!
## It should be the same from 'Google Drive' onwards.
lcFogo<-raster("/Users/quinnwebber/Google Drive/Fogo/Data/Landcover/FOGOSDSS_RS.tif") 
Legend<-fread("/Users/quinnwebber/Google Drive/Fogo/Data/Landcover/Legend.csv", header=T, sep=",", quote="",fill=TRUE)

## If you want to look at the mao:
spplot(lcFogo)

## read in functions
source("functions/ExtractPoints.R")

### NOTE: everything needs to run by ID, except random steps
## Easting = x axis = x coord = east to west = longitude
## Northing = y axis = ycoord = north to south = latitude

## extract habitat type at end step
DT[, habitat := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2),
                                          raster = lcFogo)] 

## rename habitat types
DT$habitat[DT$habitat == "1"] <- "Wetland" ## Wetland
DT$habitat[DT$habitat == "2"] <- "Broadleaf" ## Broadleaf
DT$habitat[DT$habitat == "3"] <- "ConiferForest" ## Conifer forest
DT$habitat[DT$habitat == "4"] <- "ConiferScrub" ## conifer scrub
DT$habitat[DT$habitat == "5"] <- "MixedWood" ## mixed wood
DT$habitat[DT$habitat == "6"] <- "Rock" ## rock
DT$habitat[DT$habitat == "7"] <- "Water" ## water
DT$habitat[DT$habitat == "8"] <- "Lichen" ## lichen
DT$habitat[DT$habitat == "9"] <- "Anthro" ## antrho

## check number of fixes by habitat type 
#note how there are very few broadleaf and mixedwood
DT[, .N, by = "habitat"]


### This next chunk is to calculate the proportion of each habitat type in a given radius
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
### buffer that is habitat x". Tends to make analyses more robust and less susceptible
### to problems with autocorrelation.-MPL

## Generate buffer size
buff <- 100 ## you can change this number, but right now it is a 100m circle around each point

openMoveBuff <- focalWeight(openMove, d = buff, type='circle')
ForestBuff <- focalWeight(Forest, d = buff, type='circle')
LichenBuff <- focalWeight(Lichen, d = buff, type='circle')

openMoveBuff100 <- focal(openMove,openMoveBuff,na.rm=TRUE,pad=TRUE,padValue=0)
ForestBuff100 <- focal(Forest,ForestBuff,na.rm=TRUE,pad=TRUE,padValue=0)
LichenBuff100 <- focal(Lichen,LichenBuff,na.rm=TRUE,pad=TRUE,padValue=0)

## Proportion of habitat at each relocation
ptsFogo <- SpatialPoints(data.frame(DT$EASTING,DT$NORTHING))

## extract proportion of each habitat type
DT$propOpenMove <- raster::extract(openMoveBuff100, ptsFogo)
DT$propForest <- raster::extract(ForestBuff100,ptsFogo)
DT$propLichen <- raster::extract(LichenBuff100,ptsFogo)

str(DT)

saveRDS(r1, "output/2-clean-all-rdm.RDS")


