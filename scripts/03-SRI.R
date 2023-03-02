# === SRI ---------------------------------------------------------------------


# Packages ----------------------------------------------------------------
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
DT <- readRDS('output/02-habitat-locs.Rds')
alloc.col(DT)


# Functions ---------------------------------------------------------------



# Proximity based social networks -----------------------------------------
## Temporal grouping 
group_times(DT, datetime = 'datetime', threshold = '5 minutes')

## Spatial grouping
group_pts(
  DT,
  threshold = 50,
  splitBy = 'Year',
  timegroup = 'timegroup',
  id = id,
  coords = coords
)


# Calculate SRI for each year ---------------------------------------------
nets <- get_sri(DT, id = id, by = 'Year')[!is.na(sri)]

# Set dyad id
dyad_id(nets, 'ID1', 'ID2')


# Output ------------------------------------------------------------------
saveRDS(nets, 'output/03-sri.Rds')
saveRDS(DT, 'output/03-grouped-locs.Rds')
