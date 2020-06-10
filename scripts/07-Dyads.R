# === Dyads ---------------------------------------------------------------


# Packages ----------------------------------------------------------------
libs <- c('raster', 'data.table', 'spatsoc', 'rgdal', 'landscapemetrics')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
DT <- readRDS('output/04-nn-locs')
alloc.col(DT)

landcover <- raster('../nl-landcover/output/fogo_lc.tif')
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')

openProp <- raster('output/02-open-proportion.tif')
closedProp <- raster('output/02-closed-proportion.tif')
shannon <- raster('output/02-shannon.tif')

# Put LastLoc data in binary O-1, censored data are incomplete data used in 
# survival analysis
DT[(lastLoc), censored := 0]
DT[!(lastLoc), censored := 1]
DT[, .N, censored]


# Calc previous timegroup for downstream when NN is NA
DT[, prevTimegrpNNNA := shift(timegroup), by = ANIMAL_ID]


# ====== LANDSCAPE METRICS ======= #

# Dyad centroid -----------------------------------------------------------
# For each dyad * timegroup
DT[, c('meanX', 'meanY') := lapply(.SD, mean), 
   .SDcols = c('EASTING', 'NORTHING'), by = .(dyadID, timegroup)]


# Extract land cover at centroid ------------------------------------------
DT[, dyadValue := extract(landcover, matrix(c(meanX, meanY), ncol = 2))]

# Contiguity metrics
#check data
check_landscape(landcover)

# Extract Contiguity index at centroid-------------------------------------
# Assign patch metrics (contiguity) to each cell
pcontigrst <- spatialize_lsm(landcover, 'patch', 'contig')[[1]][[1]]
weightcontig <- focalWeight(landcover, d = 100, type = 'circle')
contig <- focal(pcontigrst, weightcontig,na.rm=TRUE, pad=T)
DT[, pcontig := extract(contig, matrix(c(meanX, meanY), ncol = 2))]

# Extract shannon index at centroid----------------------------------------
DT[, ShanIndex := extract(shannon, matrix(c(meanX, meanY), ncol = 2))]

# Chech correlation landscape metrics
cor.test(DT$ShanIndex,DT$pcontig, method='pearson')
#correlated but non redundant: spatial configuration vs composition  

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



# ====== REARRANGEMENT for fission/fusion  ====== #

# Unique dyads and NN=NA --------------------------------------------------
# Get the unique dyads by timegroup
dyadNN <- unique(DT[!is.na(NN)], by = c('timegroup', 'dyadID'))

# one dyad - one runCount - one habitat percentage (for survival analysis)
# TODO -- QW: runid isn't defined before this point so get an error.
dyadNN[, mean_open := mean(propOpen, na.rm = TRUE), by = .(runid, dyadID)]

# dominant habitat during the consecutive fixes dyads spent together 
dyadNN[mean_open > 0.5, DyadDominantLC := "open"]
dyadNN[mean_open < 0.5, DyadDominantLC := "closed"]

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

# Get where NN was NA
dyadNA <- DT[is.na(NN)]

dyadNA[, c('start', 'end', 'min2') := FALSE]

dyadNA[, shifttimegrp := prevTimegrpNNNA]

# Combine where NN is NA
dyads <- rbindlist(list(dyadNN, dyadNA), fill = TRUE)

# Calculate fusion 0 ------------------------------------------------------
## Fusion 0 = 
##   a) fussion events where dyads are together > 1 consecutive relocations
##   or b) individuals where NN = NA
dyads[, fusion0 := ((start) & (min2)) | is.na(NN)]

# TODO: consider dropping where nearest neighbour distance was greater than some maximum (no opportunity to be social)


# Start/Stop --------------------------------------------------------------
dyads[, dyadPropOpenStop := shift(dyadPropOpen), by = .(runid, dyadID)]  # by dyadID only nop?
dyads[, pcontigStop := shift(pcontig), by = .(runid, dyadID)]
dyads[, ShannonStop := shift(ShanIndex), by = .(runid, dyadID)]

# TODO: adjust timegroup for dyads when observations are sequential? use prev timegroup instead?

intervals <- dyads[, .(
  ANIMAL_ID,
  Year,
  NN,
  dyadID, 
  start = shifttimegrp, 
  stop = timegroup,
  start,
  end,
  min2, 
  stayedTogether = min2 & (!end),
  dyadPropOpenStop,
  runid,
  ShannonStop,
  pcontigStop
)]


setorderv(intervals,c('dyadID','stop'),1)
# seems to work but what does it do to the other columns about the order
# + some intervals are not one, quid of the shift function right after then?

intervals[,futureEvent:=shift(stayedTogether,n=1, type='lead'),by=.(dyadID,runid)] #why NA???
intervals[,pastEvent:=shift(stayedTogether,n=1, type='lag'),by=.(dyadID,runid)]



# Output ------------------------------------------------------------------
saveRDS(dyads, 'output/07-dyads.Rds')

saveRDS(intervals, 'output/07-intervals.Rds')

