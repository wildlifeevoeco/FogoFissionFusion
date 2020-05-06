
### Home Range Analyses ====

### Packages ----
libs <- c('data.table', 'sp', 'adehabitatHR')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS('output/1-clean-all.Rds')

### Calculate Home range area for each individual ----
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

coords <- c('EASTING', 'NORTHING')
pts <- SpatialPointsDataFrame(locs[, ..coords],
                              proj4string = CRS(utm21N),
                              data = locs[, .(IDYr)])

ud <- kernelUD(pts, grid = 700, extent = 3)
vertices <- getverticeshr(ud, 95)
vert.dt <- as.data.table(vertices)

# Split up paste ID
vert.dt[, c('ANIMAL_ID', 'Year') := tstrsplit(id, '_')]

# convert from ha to km2
vert.dt[, areaKM2 := area/100]

### Home Range Overlap Networks ----
# Generate all homerange overlap networks
source("functions/hr_network.R")
hr.nets <- hr_network(locs, 
                      id = 'ANIMAL_ID', utm = utm21N, 
                      by = c('Year'),
                      returns = 'overlap')

# Restructure IDs for consistency
colnames(hr.nets) <- c("Year", "ID1", "ID2", "value")
hr.nets$dyad <- as.factor(paste(hr.nets$ID1, hr.nets$ID2, hr.nets$Year, sep = "_"))

### Output ----
saveRDS(hr.nets, 'output/5-hro.Rds')
