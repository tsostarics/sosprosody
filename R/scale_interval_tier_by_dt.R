#' Scale interval tier by DurationTier
#'
#' Part of `scale_textgrid_by_dt`, this is the procedure for a singular
#' interval tier
#'
#' @param int_tier Interval tier, i.e., `tg[[x]]`
#' @param durationtier DurationTier object from `dt.read`
#'
#' @return Interval tier with new t1 and t2 values
scale_interval_tier_by_dt <- function(int_tier,
                                      durationtier) {
  n_t2 <- length(int_tier[['t2']])
  stopifnot(length(int_tier[['t1']]) == n_t2)

  # note: add check for when there's only 1 boundary

  subareas <- int_tier[['t1']][NA][-1L]
  new_t2 <- int_tier[['t2']]


  for (i in seq_len(n_t2)) {
    tmin <- int_tier[['t1']][i]
    tmax <- int_tier[['t2']][i]
    change_times <- seq(i, n_t2)
    new_t2[change_times] <-
      new_t2[change_times] +
      dt_getarea(durationtier, tmin, tmax)
  }

  new_t1 <- c(int_tier[['t1']][1L], new_t2[-n_t2])

  int_tier[['t1']] <- new_t1
  int_tier[['t2']] <- new_t2

  int_tier
}
