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
DT[(lastLoc), censored := 0]
DT[!(lastLoc), censored := 1]
DT[, .N, censored]


# Shift type: lead
# Look "forwards" so NAs are at the bottom
# Shift is the next and not the previous
shifttype <- 'lead'


# Preserve shifted timegroup for each individual
#  when dyads are NA, this will tell us when IDs were observed last
DT[, shiftTimeWithinID := data.table::shift(timegroup, type = shifttype), 
   by = ANIMAL_ID]



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



# Unique dyads and NN=NA --------------------------------------------------
# Get the unique dyads by timegroup
dyadNN <- unique(DT[!is.na(NN)], by = c('timegroup', 'dyadID'))[, 
            .(Year,ANIMAL_ID, NN, dyadID, censored,datetime, timegroup,dyadLC,
              ShanIndex, dyadPropOpen, dyadPropClosed)]

# Set order explicitly
setorder(dyadNN, timegroup)



# Count consecutive relocations together ----------------------------------
# Shift the timegroup within dyadIDs
dyadNN[, shifttimegrp := data.table::shift(timegroup, type = shifttype), 
       by = dyadID]


# Difference between consecutive timegroups for each dyadID
# where difftimegrp == 1, the dyads remained together in consecutive timegroups
dyadNN[, difftimegrp := shifttimegrp - timegroup]

# dyadrun = binary, is it a run of at least one relocation
dyadNN[, dyadrun := difftimegrp == 1 & !is.na(difftimegrp), by = dyadID]

# nObs = how many rows for each dyadID
dyadNN[, nObs := .N, by = dyadID]


# dyadrunid = WITHIN DYAD ID - run id
# dyadrunid = 1st generate a run length id over dyad run
#             eg. dyad run = TRUE, TRUE, FALSE, TRUE
#                 dyadrunid = 1, 1, 2, 3
dyadNN[, dyadrunid := rleid(dyadrun), by = dyadID]

# then catch where potentially the difference in timegroup between rows
# is consecutively 2, 3, 4 anything > 1 
# eg. difftimegrp 2, 2, 2 would have been tagged as the same dyadrunid
# solution: generate a sequence starting from the nObs
#           with the length out = to the number of rows that 
#           arent TRUE for dyadrun
#           starting at nObs to avoid risk of overlap
dyadNN[!(dyadrun), dyadrunid := seq.int(nObs[[1]], length.out = .N),
       by = dyadID]


# N consecutive observations of dyadIDs
dyadNN[, runCount := .N, by = .(dyadrunid, dyadID)]


# Flag start and end locs for each dyad -----------------------------------
# Dont consider where runs are less than 2 relocations
dyadNN[runCount >= 2, start := timegroup == min(timegroup), 
       by = .(dyadrunid, dyadID)]

dyadNN[runCount >= 2, end := timegroup == max(timegroup), 
       by = .(dyadrunid, dyadID)]

dyadNN[runCount < 2 | is.na(runCount), c('start', 'end') := FALSE]

# if runCount is minimum 2, dyad stayed together (min2) = TRUE
dyadNN[, min2 := runCount >= 2]


# Dyad habitat ------------------------------------------------------------
# one dyad - one runCount - one habitat percentage (for survival analysis)
dyadNN[, mean_open := mean(dyadPropOpen, na.rm = TRUE), by = .(dyadrunid, dyadID)]

# dominant habitat during the consecutive fixes dyads spent together 
dyadNN[mean_open > 0.5, DyadDominantLC := "open"]
dyadNN[mean_open < 0.5, DyadDominantLC := "closed"]


# Dyad NA -----------------------------------------------------------------
# Get where NN was NA
dyadNA <- DT[is.na(NN), .(Year,ANIMAL_ID, NN, dyadID, censored, datetime, timegroup, 
                          shiftTimeWithinID, 
                          ShanIndex,dyadLC, dyadPropOpen, dyadPropClosed)]

dyadNA[, c('start', 'end', 'min2') := FALSE]

dyadNA[, shifttimegrp := shiftTimeWithinID]

# Combine where NN is NA
dyads <- rbindlist(list(dyadNN, dyadNA), fill = TRUE)


# Calculate fusion 0 ------------------------------------------------------
## Fusion 0 = 
##   a) fusion events where dyads are together >= 2 consecutive relocations
##   or b) individuals where NN = NA
dyads[, fusion0 := (start) | is.na(NN)]


# Output ------------------------------------------------------------------
saveRDS(dyads, 'output/07-dyads.Rds')

