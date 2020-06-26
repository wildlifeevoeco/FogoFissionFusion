# === Survival Analysis ---------------------------------------------------



# Packages ----------------------------------------------------------------
pkgs <- c('data.table')
lapply(pkgs, require, character.only = TRUE)


# Input -------------------------------------------------------------------
dyads <- readRDS('output/07-dyads.Rds')



# Setup Intervals ---------------------------------------------------------
# Only where dyads are minimum 2
dyadsNN <- dyads[(min2)]

intervals <- dyadsNN[, .(
  ANIMAL_ID,
  NN,
  dyadID,
  
  start = timegroup, 
  stop = shifttimegrp,
  
  Year = year(datetime),
  stayedTogether = !end,
  
  dyadPropOpen,
  ShanIndex
  )]

# Check:
# intervals[stop - start != 1]



# Output ------------------------------------------------------------------
saveRDS(intervals, 'output/07-intervals.Rds')

