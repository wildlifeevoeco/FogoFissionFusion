### Habitat ====

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'spatsoc', 
          'lubridate', 'raster', 'sp')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS('output/2-clean-all-nn.Rds')

lcFogo <- raster('../nl-landcover/output/fogo_lc.tif')
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')


### Extract habitat type ----
# TODO: why NA
DT[, Value := extract(lcFogo, matrix(c(EASTING, NORTHING), ncol = 2))]

# rename habitat types by merging legend
DT[legend, lc := Landcover, on = 'Value']

# check number of fixes by habitat type 
# note how there are very few broadleaf and mixedwood
DT[, .N, by = lc]

### Calculate the proportion of each habitat type in a given radius ----
# TODO: why set NAs to 10
# Set NAs in lc to class 10
# lcFogo[is.na(lcFogo)] <- 10

# Focal rasters
focals <- lapply(legend$Value, function(val) {
  subs(lcFogo, legend[, .(Value, Value == val)])
})
names(focals) <- legend$Value

## Combine habitat types using the Value numbers from the legend
openMove <- Reduce('+', focals[c(1, 6, 9)])
forest <- Reduce('+', focals[c(2, 3, 4, 5)])
lichen <- focals[[8]]

## Proportion of habitat in 100m buffer
# Set buffer size
buff <- 100

focweight <- focalWeight(lcFogo, d = buff, type = 'circle')

openMoveBuff100 <- focal(openMove, focweight, na.rm = TRUE, pad = TRUE, padValue = 0)
ForestBuff100 <- focal(forest, focweight, na.rm = TRUE, pad = TRUE, padValue = 0)
LichenBuff100 <- focal(lichen, focweight, na.rm = TRUE, pad = TRUE, padValue = 0)

# Proportion of habitat at each relocation
DT[, propOpenMove := extract(openMoveBuff100, matrix(c(EASTING, NORTHING), ncol = 2))]
DT[, propForest := extract(ForestBuff100, matrix(c(EASTING, NORTHING), ncol = 2))]
DT[, propLichen := extract(LichenBuff100, matrix(c(EASTING, NORTHING), ncol = 2))]


### Output ----
saveRDS(DT, 'output/3-clean-all-nn-hab.Rds')
