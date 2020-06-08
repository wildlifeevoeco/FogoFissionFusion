# === Habitat -----------------------------------------------------

# Packages ----------------------------------------------------------------
libs <- c('data.table', 'rgdal', 'raster', 'sp')
lapply(libs, require, character.only = TRUE)
library(landscapemetrics)

# Input data --------------------------------------------------------------------
DT <- readRDS('output/01-prep-locs.Rds')

lc <- raster('../nl-landcover/output/fogo_lc.tif')
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')


# Extract point land cover ------------------------------------------------
# TODO: why NA
DT[, Value := extract(lc, matrix(c(EASTING, NORTHING), ncol = 2))]

# rename habitat types by merging legend
DT[legend, lc := Landcover, on = 'Value']

# Focal rasters -----------------------------------------------------------
focals <- lapply(legend$Value, function(val) {
  subs(lc, legend[, .(Value, Value == val)])
})
names(focals) <- legend$Value


# Combine rasters ---------------------------------------------------------
# Combine land cover types using the Value numbers from the legend
open <- Reduce('+', focals[c(1, 6, 8, 9)])
closed <- Reduce('+', focals[c(2, 3, 4, 5)])


# Proportion of habitat in buffer -----------------------------------------
# Set buffer size
buff <- 100

weight <- focalWeight(lc, d = buff, type = 'circle')

openProp <- focal(open, weight, na.rm = TRUE, pad = TRUE, padValue = 0)
closedProp <- focal(closed, weight, na.rm = TRUE, pad = TRUE, padValue = 0)

# Proportion of habitat at each relocation
DT[, propOpen := extract(openProp, matrix(c(EASTING, NORTHING), ncol = 2))]
DT[, propClosed := extract(closedProp, matrix(c(EASTING, NORTHING), ncol = 2))]

# shannon index at each relocation in a new raster 
library(vegan)
shannon <- function(x, ...) {
  diversity(table(x), index="shannon")
}
weightShannon <- focalWeight(lc, d = 200, type = 'circle')
shanOut <- focal(lc, weightShannon, fun=shannon, pad=T)

DT[, ShannonIdx := extract(shanOut, matrix(c(EASTING, NORTHING), ncol = 2))]




# Summary -----------------------------------------------------------------
DT[, .N, by = lc]

# Output ------------------------------------------------------------------
saveRDS(DT, 'output/02-habitat-locs.Rds')
writeRaster(openProp, 'output/02-open-proportion.tif', overwrite=TRUE)
writeRaster(closedProp, 'output/02-closed-proportion.tif', overwrite=TRUE)
writeRaster(shanOut,'output/02-shannon.tif', overwrite=TRUE)
