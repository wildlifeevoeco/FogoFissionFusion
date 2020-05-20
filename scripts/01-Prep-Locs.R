# === Prep Locs -----------------------------------------------------------

# Packages ----------------------------------------------------------------
libs <- c('lubridate', 'data.table', 'ggplot2', 'rgdal')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
fogo <- fread(paste0('input/FogoCaribou.csv'))


# Set variables -----------------------------------------------------------
# Bounds
lowEastFogo <- 690000; highEastFogo <- 800000
lowNorthFogo <- 5450000; highNorthFogo <- 6000000

# Time zone 
tz <- 'America/St_Johns'

# Max moverate
maxMoveRate <- 30000

# Projected columns
projCols <- c('EASTING', 'NORTHING')

# Projection
# TODO: fix this proj4string
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

# Prep Fogo data to merge 
fogo[, idate := as.IDate(idate, tz = tz)]
fogo[, itime := as.ITime(itime, tz = tz)]
fogo[, datetime := as.POSIXct(paste(idate,itime), 
                              format = '%Y-%m-%d %H:%M:%S', tz = tz)]

# ID by Yr
fogo[, IDYr := paste(ANIMAL_ID, Year, sep = '_')]

# Winter season (Jan 1 to March 16)
fogo[JDate >= 1 & JDate <= 75, season := 'winter']
fogo <- fogo[!is.na(season)]

# Add relocation ID by individual and determine last loc for each individual
setorder(fogo, 'datetime')

fogo[, locID := rleid(datetime), by = ANIMAL_ID]
fogo[, lastLoc := locID == max(locID), by = ANIMAL_ID]


# UTM zone 21N
fogo[, (projCols) := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]
fogo <- fogo[(lowEastFogo < EASTING & EASTING < highEastFogo) &
               (lowNorthFogo < NORTHING & EASTING < highNorthFogo)]

# Subset ------------------------------------------------------------------
# Remove  animals with malfunctioning collars
dropIDYr <- c(
  'FO2016006_2017', # not enough fixes 
  'FO2016006_2018', # not enough fixes
  'FO2017006_2019', # not enough fixes
  'FO2017013_2018', # Dead animal
  'FO2017004_2019', # not enough fixes
  'FO2017007_2019'  # not enough fixes
)

dropID <- c(
  'FO2016001' # Dead animal
)

fogo <- fogo[!(IDYr %in% dropIDYr) &
               !(ANIMAL_ID %in% dropID)]

# Round FO2016014 datetime to nearest hour to match fixes for rest of individuals
fogo[, hour := hour(as.ITime(itime))]
is.odd <- function(x) x %% 2 != 0 
fogo$hour <- is.odd(fogo$hour)
fogo <- fogo[hour != 'TRUE' ][, hour := NULL]
fogo[, datetime := floor_date(datetime, '1 hour')]
fogo[, itime := as.ITime(datetime)]



# Summary -----------------------------------------------------------------
aa <- fogo[, .N, by = .(ANIMAL_ID, Year)]


# Output  -----------------------------------------------------------------
saveRDS(fogo, 'output/01-prep-locs.Rds')
