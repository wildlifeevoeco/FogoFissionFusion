# === Prep Locs -----------------------------------------------------------

# Packages ----------------------------------------------------------------
libs <- c('lubridate', 'data.table', 'ggplot2', 'rgdal')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
fogo <- fread(fogo_path)



# Prepare -----------------------------------------------------------------
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

fogo[, locID := rleid(datetime), by = .(ANIMAL_ID, Year)]
fogo[, lastLoc := locID == max(locID), by = .(ANIMAL_ID, Year)]


# UTM zone 21N
fogo[, (projCols) := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]
fogo <- fogo[(lowEastFogo < EASTING & EASTING < highEastFogo) &
               (lowNorthFogo < NORTHING & EASTING < highNorthFogo)]

# Subset ------------------------------------------------------------------
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
