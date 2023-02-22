#' Homerange Networks
#' 
#' Author: Alec Robitaille
#'
#' Build home range networks using `adehabitatHR::kerneloverlap` and returns either graph statitics or home range overlap. 
#' 
#' `DT` provided with columns EASTING, NORTHING for UTM coordinates. `by` argument used to specify grouping. Defaults only (as used in the paper). 
#' 
#' @param DT `data.table`` of relocations.
#' @param id individual identifier column name. 
#' @param crs crs string for passing to CRS() of the format "+init=epsg:4326"
#' @param by columns in input DT to split home range network generation and comparison by. For example: c('season', 'year') or 'herd'. Expects character vector. 
#' @param returns either 'network-stats' or 'overlap'. See Details. 
#' 
#' @return graph strength for each individual
#' @export
hr_network <- function(DT = NULL, id = NULL, coords = NULL, crs = NULL, by = NULL, returns = NULL) {
  # NSE
  value <- NULL
  
  if (is.null(DT) | is.null(id) | is.null(coords) | is.null(crs)) {
    stop('DT, id, coords and crs must be provided')
  }
  
  if (is.null(returns) | !(returns %in% c('network-stats', 'overlap'))) {
    stop('must specify return type either "network-stats" or "overlap"')
  } 
  
  if (returns == 'network-stats') {
    
    DT[, {
      KOver <- build_hr_net(.SD, id = id, crs = crs)
      hr.grph_df <-
        igraph::graph.adjacency(KOver,
                                mode = "undirected",
                                diag = FALSE,
                                weighted = TRUE)
      list(strength = igraph::graph.strength(hr.grph_df),
           ID = names(igraph::degree(hr.grph_df)))
    }, by = by, .SDcols = c(coords, by, id)]
    
  } else if (returns == 'overlap') {
    
    DT[, {
      KOver <- build_hr_net(.SD, id = id, crs = crs, coords = coords)
      out.dt <-
      data.frame(col = rep(colnames(KOver), each = nrow(KOver)), 
                 row = rep(rownames(KOver), ncol(KOver)), 
                 value = as.vector(KOver))
      
    }, by = by, .SDcols = c(coords, by, id)]
  }
}

#' @import data.table
build_hr_net <- function(DT, id, crs, coords) {
  xy <- sp::SpatialPointsDataFrame(
    coords = DT[, .SD, .SDcols = coords],
    proj4string = sp::CRS(crs),
    data = DT[, .SD, .SDcols = id])

  KOver = adehabitatHR::kerneloverlap(xy,
                                      method = "UDOI",
                                      percent = 95,
                                      grid = 700)

  KOver <- as.matrix(KOver)
  diag(KOver) <- NA
  KOver[lower.tri(KOver)] <- NA
  return(KOver)
}

