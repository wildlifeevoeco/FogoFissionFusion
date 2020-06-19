# === Dyads ---------------------------------------------------------------

 
# Packages ----------------------------------------------------------------
libs <- c('raster', 'data.table', 'spatsoc', 'rgdal', 'landscapemetrics')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
DT <- readRDS('output/04-nn-locs')
alloc.col(DT)

landcover <- raster('../nl-landcover/output/fogo_lc.tif')
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')

openFocal <- raster('output/02-open-proportion.tif')
closedFocal <- raster('output/02-closed-proportion.tif')
shannon <- raster('output/02-shannon.tif')

# Put LastLoc data in binary O-1, censored data are incomplete data used in 
# survival analysis
# TODO: 
DT[(lastLoc), censored := 0]
DT[!(lastLoc), censored := 1]
DT[, .N, censored]


# Preserve shifted timegroup for each individual
#  when dyads are NA, this will tell us when IDs were observed last
DT[, shiftTimeWithinID := shift(timegroup), by = ANIMAL_ID]


# ====== LANDSCAPE METRICS ======= #

# Dyad centroid -----------------------------------------------------------
# For each dyad * timegroup
DT[, c('meanX', 'meanY') := lapply(.SD, mean), 
   .SDcols = c('EASTING', 'NORTHING'), by = .(dyadID, timegroup)]


# Extract land cover at centroid ------------------------------------------
DT[, dyadValue := extract(landcover, matrix(c(meanX, meanY), ncol = 2))]


# Extract shannon index at centroid----------------------------------------
DT[, ShanIndex := extract(shannon, matrix(c(meanX, meanY), ncol = 2))]


# Proportion of habitat at centroid
DT[, dyadPropOpen := extract(openFocal, matrix(c(meanX, meanY), ncol = 2))]
DT[, dyadPropClosed := extract(closedFocal, matrix(c(meanX, meanY), ncol = 2))]

# rename habitat types by merging legend
DT[legend, dyadLC := Landcover, on = 'dyadValue == Value']

# Open vs closed (for the survival analysis)
DT[Value %in% c(1, 6, 7, 8, 9), habitat := "open"]
DT[Value %in% c(2, 3, 4, 5), habitat := "closed"]

# Check 
# TODO: NA
DT[, .N, habitat]



# ====== REARRANGEMENT for fission/fusion  ====== #

# Unique dyads and NN=NA --------------------------------------------------
# Get the unique dyads by timegroup
dyadNN <- unique(DT[!is.na(NN)], by = c('timegroup', 'dyadID'))[, 
            .(ANIMAL_ID, NN, dyadID, datetime, timegroup,
              dyadPropOpen, dyadPropClosed)]

# Set order explicitly
setorder(dyadNN, timegroup)

# Count consecutive relocations together ----------------------------------
# Difference between consecutive timegroups for each dyadID
# where difftimegrp == 1, the dyads remained together in consecutive timegroups
dyadNN[, difftimegrp := timegroup - data.table::shift(timegroup)]

# Dyad run id
dyadNN[, dyadrun := rleid(difftimegrp), by = dyadID]

# N consecutive observations of dyadIDs
dyadNN[, runCount := fifelse(difftimegrp == 1, .N, NA_integer_), by = .(dyadrun, dyadID)]


# Count number of timegroups dyads are observed together ------------------
dyadNN[, nObs := .N, by = .(dyadID)]


# Flag start and end locs for each dyad -----------------------------------
# Dont consider where runs are less than 2 relocations
dyadNN[runCount >= 2, start := fifelse(timegroup == min(timegroup), TRUE, FALSE), 
       by = .(dyadrun, dyadID)]

dyadNN[runCount >= 2, end := fifelse(timegroup == max(timegroup), TRUE, FALSE), by = .(dyadrun, dyadID)]

dyadNN[runCount < 2 | is.na(runCount), c('start', 'end') := FALSE]

# if runCount is minimum 2, dyad stayed together (min2) = TRUE
dyadNN[, min2 := fifelse(runCount >= 2 & !is.na(runCount), TRUE, FALSE)]


# Dyad habitat ------------------------------------------------------------
# one dyad - one runCount - one habitat percentage (for survival analysis)
dyadNN[, mean_open := mean(dyadPropOpen, na.rm = TRUE), by = .(dyadrun, dyadID)]

# dominant habitat during the consecutive fixes dyads spent together 
dyadNN[mean_open > 0.5, DyadDominantLC := "open"]
dyadNN[mean_open < 0.5, DyadDominantLC := "closed"]


# Dyad NA -----------------------------------------------------------------
# Get where NN was NA
dyadNA <- DT[is.na(NN), .(ANIMAL_ID, NN, dyadID, datetime, timegroup, shiftTimeWithinID)]

dyadNA[, c('start', 'end', 'min2') := FALSE]

dyadNA[, shifttimegrp := shiftTimeWithinID]

# Combine where NN is NA
dyads <- rbindlist(list(dyadNN, dyadNA), fill = TRUE)




# Calculate fusion 0 ------------------------------------------------------
## Fusion 0 = 
##   a) fussion events where dyads are together >= 2 consecutive relocations
##   or b) individuals where NN = NA
dyads[, fusion0 := ((start)) | is.na(NN)]

# Start/Stop --------------------------------------------------------------
# TODO adjust start stop
dyads[, dyadPropOpenStop := shift(dyadPropOpen), by = .(dyadrun, dyadID)]  
# by dyadID only nop?
dyads[, ShannonStop := shift(ShanIndex), by = .(dyadrun, dyadID)]

# TODO: adjust timegroup for dyads when observations are sequential? use prev timegroup instead?

# remove NAs for NN
dyads <- dyads[!is.na(NN)]

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
  dyadrun,
  ShannonStop
)]


setorderv(intervals,c('dyadID','stop'),1)
# seems to work but what does it do to the other columns about the order
# + some intervals are not one, quid of the shift function right after then?

intervals[,futureEvent:=shift(stayedTogether,n=1, type='lead'),by=.(dyadID,dyadrun)] #why NA???
intervals[,pastEvent:=shift(stayedTogether,n=1, type='lag'),by=.(dyadID,dyadrun)]
intervals[,.N,by=futureEvent]
intervals[,.N,by=pastEvent]
intervals[,.N,by=stayedTogether]

intervals[, FalseFission := ifelse(stayedTogether == FALSE & futureEvent == TRUE & pastEvent == TRUE,TRUE,FALSE)]
intervals[,.N,by=FalseFission]
# no true means no flse fission?

# Output ------------------------------------------------------------------
saveRDS(dyads, 'output/07-dyads.Rds')

saveRDS(intervals, 'output/07-intervals.Rds')

