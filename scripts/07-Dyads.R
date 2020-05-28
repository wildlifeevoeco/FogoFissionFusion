# === Dyads ---------------------------------------------------------------


# Packages ----------------------------------------------------------------
libs <- c('raster', 'data.table', 'spatsoc', 'rgdal')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
DT <- readRDS('output/04-nn-locs')
alloc.col(DT)

landcover <- raster('../nl-landcover/output/fogo_lc.tif')
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')

openProp <- raster('output/02-open-proportion.tif')
closedProp <- raster('output/02-closed-proportion.tif')


# Put LastLoc data in binary O-1, censored data are incomplete data used in 
# survival analysis
DT[(lastLoc), censored := 0]
DT[!(lastLoc), censored := 1]
DT[,.N,censored]


# Calc previous timegroup for downstream when NN is NA
DT[, prevTimegrpNNNA := shift(timegroup), by = ANIMAL_ID]


# Dyad centroid -----------------------------------------------------------
# For each dyad * timegroup
DT[, c('meanX', 'meanY') := lapply(.SD, mean), 
   .SDcols = c('EASTING', 'NORTHING'), by = .(dyadID, timegroup)]


# Extract land cover at centroid ------------------------------------------
DT[, dyadValue := extract(landcover, matrix(c(meanX, meanY), ncol = 2))]

# Proportion of habitat at centroid
DT[, dyadPropOpen := extract(openProp, matrix(c(meanX, meanY), ncol = 2))]
DT[, dyadPropClosed := extract(closedProp, matrix(c(meanX, meanY), ncol = 2))]

# rename habitat types by merging legend
DT[legend, dyadLC := Landcover, on = 'dyadValue == Value']

# Open vs closed (for the survival analysis)
DT[Value %in% c(1, 6, 8, 9), habitat := "open"]
DT[Value %in% c(2, 3, 4, 5), habitat := "closed"]

# Check 
DT[, .N, habitat]

# Unique dyads and NN=NA --------------------------------------------------
# Get the unique dyads by timegroup
dyadNN <- unique(DT[!is.na(NN)], by = c('timegroup', 'dyadID'))

# Set the order of the rows
setorder(dyadNN, timegroup)


# Count number of timegroups dyads are observed together ------------------
dyadNN[, nObs := .N, by = .(dyadID)]


# Count consecutive relocations together ----------------------------------
# Shift the timegroup within dyadIDs
dyadNN[, shifttimegrp := data.table::shift(timegroup, 1), by = ANIMAL_ID]

# Difference between consecutive timegroups for each dyadID
# where difftimegrp == 1, the dyads remained together in consecutive timegroups
dyadNN[, difftimegrp := timegroup - shifttimegrp]


# Run id of diff timegroups
dyadNN[, runid := rleid(difftimegrp), by = dyadID]

# N consecutive observations of dyadIDs
dyadNN[, runCount := fifelse(difftimegrp == 1, .N, NA_integer_), by = .(runid, dyadID)]


# Flag start and end locs for each dyad -----------------------------------
# Dont consider where runs are less than 2 relocations
dyadNN[runCount > 1, start := fifelse(timegroup == min(timegroup), TRUE, FALSE), by = .(runid, dyadID)]

dyadNN[runCount > 1, end := fifelse(timegroup == max(timegroup), TRUE, FALSE), by = .(runid, dyadID)]

dyadNN[runCount <= 1 | is.na(runCount), c('start', 'end') := FALSE]

## if runCount is minimum 2, dyad stayed together (min2) = TRUE
dyadNN[, min2 := fifelse(runCount >= 2 & !is.na(runCount), TRUE, FALSE)]


# one dyad - one runCount - one habitat percentage (for survival analysis)
dyadNN[, mean_open := mean(propOpen, na.rm = TRUE), by = .(runid, dyadID)]

# dominant habitat during the consecutive fixes dyads spent together put in 07-dyad
dyadNN[mean_open > 0.5, DyadDominantLC := "open"]
dyadNN[mean_open < 0.5, DyadDominantLC := "closed"]


# Get where NN was NA
dyadNA <- DT[is.na(NN)]

dyadNA[, c('start', 'end', 'min2') := FALSE]

# Combine where NN is NA
dyads <- rbindlist(list(dyadNN, dyadNA), fill = TRUE)

# Calculate fusion 0 ------------------------------------------------------
## Fusion 0 = 
##   a) fussion events where dyads are together > 1 consecutive relocations
##   or b) individuals where NN = NA
dyads[, fusion0 := ((start) & (min2)) | is.na(NN)]

# TODO: consider dropping where nearest neighbour distance was greater than some maximum (no opportunity to be social)


# Start/Stop --------------------------------------------------------------
dyads[, meanOpenStop := shift(mean_open), by = .(runid, dyadID)]
dyads[, dyadPropOpenStop := shift(dyadPropOpen), by = .(runid, dyadID)]

# TODO: why FO2016011 start stop same
intervals <- dyads[, .(
  ANIMAL_ID,
  NN,
  dyadID, 
  start = shifttimegrp, 
  stop = timegroup,
  start,
  end,
  min2, 
  stayedTogether = min2 & (!end),
  meanOpenStop,
  dyadPropOpenStop,
  DyadDominantLC,
  runid
)]


# Output ------------------------------------------------------------------
saveRDS(dyads, 'output/07-dyads.Rds')

