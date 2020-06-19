# === Survival Analysis ---------------------------------------------------



# Packages ----------------------------------------------------------------
pkgs <- c('data.table')
lapply(pkgs, require, character.only = TRUE)


# Input -------------------------------------------------------------------
dyads <- readRDS('output/07-dyads.Rds')


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
saveRDS(intervals, 'output/07-intervals.Rds')

