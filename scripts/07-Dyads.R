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


# try edge density
sample_points= matrix(c(DT$meanX,DT$meanY), ncol=2)
ED=sample_lsm(landcover, y=sample_points, shape='circle', size=200,what='lsm_l_ed', progress=TRUE)


# Unique dyads and NN=NA --------------------------------------------------
# check where ID and NN differ in timegroups
dyadids <- DT[!is.na(dyadID), .(dID = ANIMAL_ID, dNN = NN), dyadID][, .SD[[1]], dyadID]

diffs <- dyadids[, c(setdiff(DT[ANIMAL_ID == dID, unique(timegroup)],
                              DT[ANIMAL_ID == dNN, unique(timegroup)]),
                      setdiff(DT[ANIMAL_ID == dNN, unique(timegroup)],
                              DT[ANIMAL_ID == dID, unique(timegroup)])),
                  by = dyadID]

diffs[, difnot1 := (data.table::shift(V1, type = shifttype) - V1) != 1,
       dyadID]

miss <- diffs[(difnot1), .(dyadID, timegroup = V1)]
miss[, c('ANIMAL_ID', 'NN') := tstrsplit(dyadID, '-')]
miss[, potentialmiss := TRUE]

# Get the unique dyads by timegroup
dyadNN <- unique(DT[!is.na(NN)], by = c('timegroup', 'dyadID'))[, 
            .(Year, ANIMAL_ID, NN, dyadID, censored, datetime, timegroup,
              dyadLC, ShanIndex, dyadPropOpen, dyadPropClosed)]

dyadMiss <- rbindlist(list(dyadNN, miss), fill = TRUE)


# Set order explicitly
setorder(dyadMiss, timegroup)

# Check difference between previous timegroup and next timegroup
# if 2, then this relocation is within the stream of relocations
dyadMiss[, dif2 := shift(timegroup, type = 'lead') - 
            shift(timegroup, type = 'lag'),
       by = dyadID]

# Check if potential misses are missed: if the dif2 is equal to 2 and it is
#  a potential miss
dyadMiss[, missed := potentialmiss & dif2 == 2]

# Drop dif2 that overestimates observations of dyads
dyadMiss[, dif2 := NULL]

# Only preserve where missed is TRUE or NA
dyads <- dyadMiss[(missed) | is.na(potentialmiss)]

# Recalculate dif2 after dropping extra missed
dyads[, dif2 := shift(timegroup, type = 'lead') - 
            shift(timegroup, type = 'lag'),
         by = dyadID]


# Count consecutive relocations together ----------------------------------
# Shift the timegroup within dyadIDs
dyads[, shifttimegrp := data.table::shift(timegroup, type = shifttype), 
       by = dyadID]


# Difference between consecutive timegroups for each dyadID
# where difftimegrp == 1, the dyads remained together in consecutive timegroups
dyads[, difftimegrp := shifttimegrp - timegroup]


# dyadrun = binary, is it a run of at least one relocation
dyads[, dyadrun := difftimegrp == 1 & !is.na(difftimegrp), by = dyadID]


# nObs = how many rows for each dyadID
dyads[, nObs := .N, by = dyadID]


# withinID = WITHIN DYAD ID - run id
# withinID = 1st generate a run length id over dyad run
#             eg. dyad run = TRUE, TRUE, FALSE, TRUE
#                 dyadrunid = 1, 1, 2, 3
dyads[, withinID := rleid(dyadrun), by = dyadID]

# then catch where potentially the difference in timegroup between rows
# is consecutively 2, 3, 4 anything > 1 
# eg. difftimegrp 2, 2, 2 would have been tagged as the same dyadrunid
# solution: generate a sequence starting from the nObs
#           with the length out = to the number of rows that 
#           arent TRUE for dyadrun
#           starting at nObs to avoid risk of overlap
dyads[!(dyadrun), withinID := seq.int(nObs[[1]], length.out = .N),
       by = dyadID]


# ACROSS DYAD - RUN ID
dyads[, dyadrunid := .GRP, .(withinID, dyadID)]


# N consecutive observations of dyadIDs
dyads[, runCount := .N, by = .(dyadrunid, dyadID)]


# False fission -----------------------------------------------------------
# False fission events where not relevant to missed or potential missed
#  and difference between previous and next timegroup is 2
#  and incorrectly classified as not in a dyad run

dyads[, falsefission := dif2 == 2 & !dyadrun & is.na(potentialmiss)]

dyads[, .N, .(falsefission, missed)]

# Flag start and end locs for each dyad -----------------------------------
# Dont consider where runs are less than 2 relocations
dyads[runCount >= 2, start := timegroup == min(timegroup), 
       by = .(dyadrunid, dyadID)]

dyads[runCount >= 2, end := timegroup == max(timegroup), 
       by = .(dyadrunid, dyadID)]

dyads[runCount < 2 | is.na(runCount), c('start', 'end') := FALSE]

# if runCount is minimum 2, dyad stayed together (min2) = TRUE
dyads[, min2 := runCount >= 2]


# Dyad habitat ------------------------------------------------------------
# one dyad - one runCount - one habitat percentage (for survival analysis)
dyads[, mean_open := mean(dyadPropOpen, na.rm = TRUE), by = .(dyadrunid, dyadID)]

# dominant habitat during the consecutive fixes dyads spent together 
dyads[mean_open > 0.5, DyadDominantLC := "open"]
dyads[mean_open < 0.5, DyadDominantLC := "closed"]


# Dyad NA -----------------------------------------------------------------
# Get where NN was NA
dyadNA <- DT[is.na(NN), .(Year,ANIMAL_ID, NN, dyadID, censored, datetime, timegroup, 
                          shiftTimeWithinID, 
                          ShanIndex, dyadLC, dyadPropOpen, dyadPropClosed)]

dyadNA[, c('start', 'end', 'min2') := FALSE]

dyadNA[, shifttimegrp := shiftTimeWithinID]

# Combine where NN is NA
out <- rbindlist(list(dyads, dyadNA), fill = TRUE)


# Calculate fusion 0 ------------------------------------------------------
## Fusion 0 = 
##   a) fusion events where dyads are together >= 2 consecutive relocations
##   or b) individuals where NN = NA
out[, fusion0 := (start) | is.na(NN)]


# Output ------------------------------------------------------------------
saveRDS(out, 'output/07-dyads.Rds')


