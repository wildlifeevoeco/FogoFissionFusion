# === Merge Outputs -------------------------------------------------------



# Packages ----------------------------------------------------------------
library(data.table)



# Input -------------------------------------------------------------------
sri <- readRDS('output/03-sri.Rds')
hro <- readRDS('output/05-hro.Rds')
body <- readRDS('output/06-body-size-diffs.Rds')



# Merge -------------------------------------------------------------------
# List data.tables
lsDTs <- list(sri, hro, body)

# Merge together
DT <- merge(sri, hro, by = c('dyadID', 'ID1', 'ID2', 'Year'))
out <- merge(DT, body, by = c('dyadID', 'ID1', 'ID2'))



# Output ------------------------------------------------------------------
saveRDS(out, 'output/09-all-dyad-data.Rds')
