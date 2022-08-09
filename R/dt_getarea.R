#' Get area for duration manipulation
#'
#' Based on RealTier_getArea from RealTier.cpp
#' https://github.com/praat/praat/blob/master/fon/RealTier.cpp
#'
#' It's very important to remember that this function gives the area **from 1**,
#' so a scaling factor of 1/3 from 0 to 1 would yield an area here
#' of -2/3 rather than 1/3. Similarly, a scaling factor of 2 from 3 to 4 would
#' return 1. This can be interpreted as the change in time necessary to yield
#' a shrunk or compressed time.
#'
#' This could technically be used for pitch tiers too, in which case you would
#' be calculating the integral of the pitch from 1. Not sure what use that
#' would have. If you use that you should add 1 to the output.
#'
#' @param durationtier DurationTier object
#' @param tmin left time to integrate from
#' @param tmax right time to integrate from
#'
#' @return Area above (+) 1 or below (-) one.
dt_getarea <- function(durationtier, tmin, tmax) {
  which_points <- durationtier[["t"]] <= tmax & durationtier[["t"]] >= tmin
  dt_points <-  durationtier[['f']][which_points]
  dt_times <- durationtier[['t']][which_points]


  dt_points <- c(get_value_at_time(durationtier, tmin),
                 dt_points,
                 get_value_at_time(durationtier, tmax))
  dt_times <- c(tmin, dt_times, tmax)

  subareas <- (dt_times[NA])[-1L]

  i_over <- seq_len(length(dt_times)-1L)

  for (i in i_over) {
    subareas[i] <-
      0.5 *
      (dt_times[i+1] - dt_times[i]) *
      (dt_points[i] + dt_points[i+1] - 2) # -2 for deviation from 1x
  }

  sum(subareas)
}
