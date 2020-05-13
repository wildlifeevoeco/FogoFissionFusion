# === SRI ---------------------------------------------------------------------


# Packages ----------------------------------------------------------------
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
DT <- readRDS('output/1-prep-locs.Rds')
alloc.col(DT)


# Functions ---------------------------------------------------------------
source('functions/get_sri.R')



# Proximity based social networks -----------------------------------------
## Temporal grouping 
group_times(DT, datetime = 'datetime', threshold = '5 minutes')

## Spatial grouping
group_pts(
  DT,
  threshold = 50,
  splitBy = 'Year',
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = c('EASTING', 'NORTHING')
)


# Calculate SRI for each year ---------------------------------------------
nets <- get_sri(DT, id = 'ANIMAL_ID', by = 'Year')[!is.na(sri)]

# Set dyad id
dyad_id(nets, 'ID1', 'ID2')


# Output ------------------------------------------------------------------
saveRDS(nets, 'output/3-sri.Rds')