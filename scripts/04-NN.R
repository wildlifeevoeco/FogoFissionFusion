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
    threshold = NULL,
    splitBy = c('Year')
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
  by.x = c('ANIMAL_ID', 'timegroup', 'Year'),
  by.y = c('ID', 'timegroup', 'Year')
)

out <- m[, .(ANIMAL_ID, NN, dyadID, idate, itime, datetime, timegroup, Year,
             season, distance, EASTING, NORTHING)]

# Output ------------------------------------------------------------------
saveRDS(out, 'output/04-nn-locs')