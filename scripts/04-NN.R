# === Calculate NN --------------------------------------------------------


# Packages ----------------------------------------------------------------
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
DT <- readRDS('output/03-grouped-locs.Rds')
alloc.col(DT)


# Set variables -----------------------------------------------------------
projCols <- c('EASTING', 'NORTHING')


# Nearest neighbour -------------------------------------------------------
edges <-
  edge_nn(
    DT = DT,
    id = 'ANIMAL_ID',
    coords = c('EASTING', 'NORTHING'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    threshold = NULL
  )


# Threshold neighbours ----------------------------------------------------
maxdist <- 500
edges[distance > maxdist, NN := NA]

# Set dyad id
dyad_id(edges, 'ID', 'NN')

# Merge -------------------------------------------------------------------
m <- merge(
  DT,
  edges,
  by.x = c('ANIMAL_ID', 'timegroup'),
  by.y = c('ID', 'timegroup')
)

out <- m[, .(ANIMAL_ID, NN, dyadID, idate, itime, datetime, timegroup, Year,
             Value, lc, propOpen, propClosed,
             season, distance, EASTING, NORTHING,
             locID, lastLoc)]

# Output ------------------------------------------------------------------
saveRDS(out, 'output/04-nn-locs')
