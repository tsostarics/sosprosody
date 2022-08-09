#' Interpolate value at time
#'
#' Given a pitch tier or duration tier and a timepoint, calculate the value
#' at that time.
#'
#' Note to self: this is a different approach to `interpolate_pitchpoints`,
#' they can and should be unified, with interpolate_pitchpoints being a
#' vectorized wrapped over this.
#'
#' @param tier_obj DurationTier or PitchTier object, not to be confused with
#' a single tier from a textgrid
#' @param t time to calculate at
#'
#' @return Numeric value (pitch or duration multiplier) at time t
#' @export
get_value_at_time <- function(tier_obj, t) {
  # Must be point tier or duration tier containing t and f
  stopifnot(all(c('t','f') %in% names(tier_obj)))
  # note this can be done faster w/ crossover algorithm
  # and can also work with pitchtiers
  which_left <- tier_obj[['t']] <= t
  which_right <- tier_obj[['t']] >= t

  none_left <- sum(which_left) == 0
  none_right <- sum(which_right) == 0

  if (none_left & none_right)
    stop("No points found on tier")

  tleft <- tier_obj[['t']][which_left][sum(which_left)]
  tright <- tier_obj[['t']][which_right][1L]

  fleft <- tier_obj[['f']][which_left][sum(which_left)]
  fright <- tier_obj[['f']][which_right][1L]

  # If there are no other points on one side, then the value
  # carries over to the boundary timestamp like so:
  # |        O----------|
  # |       /           |
  # |------O            |
  if (none_left){
    fleft <- fright
    tleft <- as.numeric(tier_obj[["tmin"]]) # usually 0
  }

  if (none_right){
    fright <- fleft
    tright <- as.numeric(tier_obj[['tmax']]) # duration of file
  }

  if (t == tleft)
    return (fleft) # == fright if tleft == tright
  if (t == tright)
    return (fright)

  dist_left <- t - tleft
  dist_right <- tright - t

  weighted.mean(c(fleft, fright),
                c(dist_right, dist_left))
}
