### Body Size ====
# Morgane Le Goff, Alec L. Robitaille

### Packages ----
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


### Input ----
body <- fread('input/body.csv')

idcol <- 'ANIMAL_ID'

### Functions ----
#' @param DT input data.table 
#' @param col column to measure difference in
#' @param id id column
#' 
#' @example
#' diff_dyad(bodyavg, 'total_length', 'ANIMAL_ID')
diff_dyad <- function(DT, col, id) {
  d <- as.matrix(dist(DT[[col]]))
  d[lower.tri(d, diag = TRUE)] <- NA
  dimnames(d) <- list(DT[[id]], DT[[id]])
  
  lendiff <- data.table(
    ID1 = rep(colnames(d), each = nrow(d)),
    ID2 = rep(rownames(d), ncol(d)),
    diff = as.vector(d)
  )[!is.na(diff)]
  setnames(lendiff, 'diff', paste0('diff_', col))
  dyad_id(lendiff, 'ID1', 'ID2')
}


### Body size ----
bodyavg <- body[, lapply(.SD, mean, na.rm = T), 
                by = idcol, .SDcols = -'date']


# Calculate difference between all individuals
varls <- bodyavg[, names(.SD), .SDcols = -idcol]

lsdiff <- lapply(varls, diff_dyad, DT = bodyavg, id = idcol)

diffs <- Reduce(function(x, y) merge(x, y, on = 'dyadID'),
                lsdiff)


## Other ways of using it:
# eg. for one variable
# diff_dyad(DT = bodyavg, col = 'total_length', id = idcol)

# one liner (nice)
# Reduce(function(x, y) merge(x, y, on = 'dyadID'), lapply(varls, diff_dyad, DT = bodyavg, id = idcol))


### Output ----
saveRDS(diffs, 'output/6-body-size-diffs.Rds')
