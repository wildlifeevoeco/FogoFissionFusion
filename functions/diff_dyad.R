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