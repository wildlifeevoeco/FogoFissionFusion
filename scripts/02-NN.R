# === Calculate NN --------------------------------------------------------


# Packages ----------------------------------------------------------------
libs <- c( 'ggplot2', 'rgdal',  'data.table',
          'spatsoc', 'igraph', 'asnipe')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
DT <- readRDS('output/03-grouped-locs.Rds')
alloc.col(DT)


# Set variables -----------------------------------------------------------
projCols <- c('EASTING', 'NORTHING')



# Nearest neighbour -------------------------------------------------------
# TODO: set max distance between NN, NN to NA
edges <-
  edge_nn(
    DT = DT,
    id = 'ANIMAL_ID',
    coords = c('EASTING', 'NORTHING'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    splitBy = c('Year')
  )



# Merge -------------------------------------------------------------------
out <- merge(
  DT,
  edges,
  by.x = c('ANIMAL_ID', 'timegroup', 'Year'),
  by.y = c('ID', 'timegroup', 'Year')
)


# Output ------------------------------------------------------------------
saveRDS(out, 'output/04-nn-locs')