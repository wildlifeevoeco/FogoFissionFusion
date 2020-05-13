# === Dyads ---------------------------------------------------------------


# Packages ----------------------------------------------------------------
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
DT <- readRDS('output/04-nn-locs')
alloc.col(DT)

lc <- raster('../nl-landcover/output/fogo_lc.tif')
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')


# Dyad centroid -----------------------------------------------------------
# For each dyad * timegroup
DT[, c('meanX', 'meanY') := lapply(.SD, mean), 
   .SDcols = c('EASTING', 'NORTHING'), by = .(dyadID, timegroup)]


# Extract land cover at centroid ------------------------------------------
DT[, Value := extract(lc, matrix(c(meanX, meanY), ncol = 2))]

# rename habitat types by merging legend
DT[legend, lc := Landcover, on = 'Value']


# Fusion and fission events -----------------------------------------------
# Get the unique dyads by timegroup
dyads <- unique(DT, by = c('timegroup', 'dyadID'))

# Set the order of the rows
setorder(dyads, timegroup)

## Count number of timegroups dyads are observed together
dyads[, nObs := .N, by = .(dyadID)]

## Count consecutive relocations together
# Shift the timegroup within dyadIDs
dyads[, shifttimegrp := shift(timegroup, 1), by =  dyadID]

# Difference between consecutive timegroups for each dyadID
# where difftimegrp == 1, the dyads remained together in consecutive timegroups
dyads[, difftimegrp := timegroup - shifttimegrp]


# Run id of diff timegroups
dyads[, runid := rleid(difftimegrp), by = dyadID]

# N consecutive observations of dyadIDs
dyads[, runCount := fifelse(difftimegrp == 1, .N, NA_integer_), by = .(runid, dyadID)]

## Start and end of consecutive relocations for each dyad
# Dont consider where runs aren't more than one relocation
dyads[runCount > 1, start := fifelse(timegroup == min(timegroup), TRUE, NA), by = .(runid, dyadID)]

dyads[runCount > 1, end := fifelse(timegroup == max(timegroup), TRUE, NA), by = .(runid, dyadID)]



