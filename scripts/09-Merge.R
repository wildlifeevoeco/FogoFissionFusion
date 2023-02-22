# === Merge Outputs -------------------------------------------------------


# Packages ----------------------------------------------------------------
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


# Input -------------------------------------------------------------------
sri <- readRDS('output/03-sri.Rds')
hro <- readRDS('output/05-hro.Rds')
body <- readRDS('output/06-body-size-diffs.Rds')


# Merge -------------------------------------------------------------------
# List data.tables
lsDTs <- list(sri, hro, body)

# Remove id columns from all data.table
idcols <- c('ID1', 'ID2')
lapply(lsDTs, function(DT) DT[, (idcols) := NULL])

# Merge together
DT <- merge(sri, hro, by = c('dyadID', 'Year'))
out <- merge(DT, body, by = 'dyadID')



# Output ------------------------------------------------------------------
saveRDS(out, 'output/6-all-dyad-data.Rds')