#' Reset tmax in textgrid
#'
#' @param textgrid
#'
#' @return
reset_tmax <- function(textgrid) {
  old_tmax <- as.numeric(attr(textgrid, 'tmax'))

  tiers_tmax <- vapply(textgrid, get_max_time, 1.0)

  has_shrunk <- all(tiers_tmax < old_tmax)

  new_tmax <- ifelse(has_shrunk, max(tiers_tmax), max(c(tiers_tmax, old_max)))

  is_interval <- vapply(textgrid, \(tier) tier[['type']]=='interval', TRUE)
  for (tier in names(textgrid[is_interval])) {
    n_t2 <- length(textgrid[[tier]][['t2']])
    textgrid[[tier]][['t2']][n_t2] <- new_tmax
  }
  textgrid
}

get_max_time <- function(tier) {
  ifelse(tier[['type']] == 'interval',
         max(tier[['t2']]),
         max(tier[['t']]))
}
