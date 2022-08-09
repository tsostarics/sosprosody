#' Scale point tier by DurationTier
#'
#' Part of `scale_textgrid_by_dt`, this is the procedure for a singular
#' point tier
#'
#' @param point_tier Singular point tier from a textgrid, i.e. `tier[[x]]`
#' @param durationtier DurationTier object from `dt.read`
#'
#' @return The same point tier but with shifted times
scale_point_tier_by_dt <- function(point_tier,
                                   durationtier) {
  n_points <- length(point_tier[['t']])
  tmin = 0
  new_t <- point_tier[['t']]
  for (i in seq_len(n_points)) {
    change_times <- seq(i, n_points)
    cur_time <- point_tier[['t']][i]
    new_t[change_times] <-
      new_t[change_times] +
      dt_getarea(durationtier, tmin, cur_time)

    tmin <- cur_time
  }

  point_tier[['t']] <- new_t

  point_tier
}
