# === Body Size -----------------------------------------------------------



# Packages ----------------------------------------------------------------
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


# Input data --------------------------------------------------------------
body <- fread('input/body.csv')


# Add columns, remove columns
body[, c('hump_girth', 'neck') := NULL]
body[, sum_heart_length := total_length + heart_girth]

# Set variables -----------------------------------------------------------
idcol <- 'ANIMAL_ID'



# Body size ---------------------------------------------------------------
# Average body size for each individual (some with multiple observations)
bodyavg <- body[, lapply(.SD, mean, na.rm = T), 
                by = idcol, .SDcols = -'date']

# Calculate difference between all individuals
varls <- bodyavg[, names(.SD), .SDcols = -idcol]

lsdiff <- lapply(varls, diff_dyad, DT = bodyavg, id = idcol)

diffs <- Reduce(function(x, y) merge(x, y, by = 'dyadID'),
                lsdiff)

## Other ways of using diff_dyad:
# eg. for one variable
# diff_dyad(DT = bodyavg, col = 'total_length', id = idcol)

# one liner (nice)
# Reduce(function(x, y) merge(x, y, on = 'dyadID'), lapply(varls, diff_dyad, DT = bodyavg, id = idcol))



# Output ------------------------------------------------------------------
saveRDS(diffs, 'output/06-body-size-diffs.Rds')

