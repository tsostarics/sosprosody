#' Scale textgrid by duration tier
#'
#' Given a textgrid and a duration tier, the interval boundaries and point
#' locations will be shifted over in accordance with sections that shrink
#' and stretch. This is most useful when you know exactly how you want
#' to manipulate the duration of a sound file but want to maintain any
#' textgrid that goes along with it in the warped file.
#'
#' @param textgrid TextGrid object from `tg.read`
#' @param durationtier DurationTier object from `dt.read`
#'
#' @return The textgrid with modified time values for each tier
scale_textgrid_by_dt <- function(textgrid, durationtier) {
  for (i in seq_along(textgrid)) {
    textgrid[[i]] <- scale_tier_by_dt(textgrid[[i]], durationtier)
  }

  reset_tmax(textgrid)
}

scale_tier_by_dt <- function(tier, durationtier) {
  if (tier[['type']] == 'interval')
    return(scale_interval_tier_by_dt(tier, durationtier))

  scale_point_tier_by_dt(tier, durationtier)
}
