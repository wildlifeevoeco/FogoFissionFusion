# === Home Range Overlap --------------------------------------------------



# Packages ----------------------------------------------------------------
libs <- c('data.table', 'sp', 'adehabitatHR', 'spatsoc')
lapply(libs, require, character.only = TRUE)



# Input data --------------------------------------------------------------
DT <- readRDS('output/01-prep-locs.Rds')



# Calculate home range area -----------------------------------------------
pts <- SpatialPointsDataFrame(DT[, ..coords],
                              proj4string = CRS(crs),
                              data = DT[, .(IDYr)])

ud <- kernelUD(pts, grid = 700, extent = 3)
vertices <- getverticeshr(ud, 95)
vert.dt <- as.data.table(vertices)

# Split up paste ID
vert.dt[, c(id, 'Year') := tstrsplit(id, '_')]

# convert from ha to km2
vert.dt[, areaKM2 := area / 100]



# Home range overlap networks ---------------------------------------------
hr.nets <- hr_network(DT, 
                      id = id, 
                      coords = coords,
                      crs = crs, 
                      by = c('Year'),
                      returns = 'overlap')[!is.na(value)]

# Restructure IDs for consistency
idcols <- c('ID1', 'ID2')
setnames(hr.nets, c('Year', idcols, 'udoi'))
hr.nets[, (idcols) := lapply(.SD, as.character), .SDcols = idcols]

# Generate dyad id
dyad_id(hr.nets, idcols[[1]], idcols[[2]])



# Output ------------------------------------------------------------------
saveRDS(hr.nets, 'output/05-hro.Rds')
