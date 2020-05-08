### Combine outputs ====


### Packages ----
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


### Input ----
sri <- readRDS('output/4-sri.Rds')
hro <- readRDS('output/5-hro.Rds')
body <- readRDS('output/6-body-size-diffs.Rds')


### Merge ----
# List data.tables
lsDTs <- list(sri, hro, body)

# Remove id columns from all data.table
idcols <- c('ID1', 'ID2')
lapply(lsDTs, function(DT) DT[, (idcols) := NULL])

# Merge together
DT <- merge(sri, hro, by = c('dyadID', 'Year'))
out <- merge(DT, body, by = 'dyadID')


### Output ----
saveRDS(out, 'output/6-all-dyad-data.Rds')
fwrite(out, 'output/6-all-dyad-data.csv')



