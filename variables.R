# === Variables -----------------------------------------------------------


# Paths
fogo_path <- 'input/FogoCaribou.csv'
lc_path <- '../nl-landcover/output/fogo_lc.tif'
legend_path <- '../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv'
body_path <- 'input/body.csv'



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



# Animals with malfunctioning collars or dead animals
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
