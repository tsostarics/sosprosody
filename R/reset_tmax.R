#' Reset tmax in textgrid
#'
#' This is intended to only be used after applying a duration manipulation
#' to a textgrid via `scale_textgrid_by_dt`. After a textgrid's duration is
#' changed, the minimum and maximum time may also need to change accordingly
#' to be earlier or later.
#'
#' Important: This function assumes that the final `t2` value in every interval
#' tier is at the maximum time (i.e., duration), but I don't think this is
#' necessarily a hard restriction on the format of TextGrids.
#'
#' @param textgrid Textgrid object
#'
#' @return The textgrid with an updated `tmax` attribute and modified
reset_tmax <- function(textgrid) {
  old_tmax <- as.numeric(attr(textgrid, 'tmax'))

  tiers_tmax <- vapply(textgrid, get_max_time, 1.0)

  has_shrunk <- all(tiers_tmax < old_tmax)

  new_tmax <- ifelse(has_shrunk, max(tiers_tmax), max(c(tiers_tmax, old_tmax)))

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
