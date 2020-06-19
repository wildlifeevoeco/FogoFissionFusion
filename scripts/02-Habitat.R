# === Habitat -----------------------------------------------------

# Packages ----------------------------------------------------------------
libs <- c('data.table', 'rgdal', 'raster', 'sp', 'vegan')
lapply(libs, require, character.only = TRUE)

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
open <- Reduce('+', focals[c(1, 6, 7, 8, 9)])
closed <- Reduce('+', focals[c(2, 3, 4, 5)])


# Proportion of habitat in buffer -----------------------------------------
# Set buffer size
buff <- 100

weight <- focalWeight(lc, d = buff, type = 'circle')

openFocal <- focal(open, weight, na.rm = TRUE, pad = TRUE, padValue = 0)
closedFocal <- focal(closed, weight, na.rm = TRUE, pad = TRUE, padValue = 0)

# Proportion of habitat at each relocation
DT[, propOpen := extract(openFocal, matrix(c(EASTING, NORTHING), ncol = 2))]
DT[, propClosed := extract(closedFocal, matrix(c(EASTING, NORTHING), ncol = 2))]

# shannon index at each relocation in a new raster 
shannon <- function(x, ...) {
  diversity(table(x), index="shannon")
}
weightShannon <- focalWeight(lc, d = 100, type = 'circle')
shanOut <- focal(lc, weightShannon, fun=shannon, pad=T)

DT[, ShannonIdx := extract(shanOut, matrix(c(EASTING, NORTHING), ncol = 2))]




# Summary -----------------------------------------------------------------
DT[, .N, by = lc]

# Output ------------------------------------------------------------------
saveRDS(DT, 'output/02-habitat-locs.Rds')
writeRaster(openFocal, 'output/02-open-proportion.tif', overwrite=TRUE)
writeRaster(closedFocal, 'output/02-closed-proportion.tif', overwrite=TRUE)
writeRaster(shanOut,'output/02-shannon.tif', overwrite=TRUE)
