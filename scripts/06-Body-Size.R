# === Body Size -----------------------------------------------------------



# Packages ----------------------------------------------------------------
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)



# Input data --------------------------------------------------------------
body <- fread('input/body.csv')


# Add columns, remove columns
body[, c('hump_girth', 'neck') := NULL]
body[, sum_heart_length := total_length + heart_girth]



# Body size ---------------------------------------------------------------
# Average body size for each individual (some with multiple observations)
bodyavg <- body[, lapply(.SD, mean, na.rm = T), 
                by = id, .SDcols = -'date']

# Calculate difference between all individuals
varls <- bodyavg[, names(.SD), .SDcols = -id]

lsdiff <- lapply(varls, diff_dyad, DT = bodyavg, id = id)

diffs <- Reduce(function(x, y) merge(x, y, by = c('dyadID', 'ID1', 'ID2')),
                lsdiff)



# Output ------------------------------------------------------------------
saveRDS(diffs, 'output/06-body-size-diffs.Rds')

