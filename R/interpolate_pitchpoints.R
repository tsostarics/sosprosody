#' Interpolate pitch points
#'
#' Given a vector of numeric timepoints `new_times` that lie within the range
#' of `old_times` and `pitch_vals` located at `old_times`, determine which
#' two pitch points are adjacent to each new timepoint, then calculate the
#' weighted mean based on how far the time point is from each pitch point to
#' interpolate between the two.
#'
#' @param new_times Sorted numeric vector with range within `old_times`
#' @param old_times Sorted numeric vector
#' @param pitch_vals Pitch values, of same length as `old_times`
#'
#' @return Numeric vector of interpolated pitch points to correspond to the
#' timepoints at `new_times`
interpolate_pitchpoints <- function(new_times, old_times, pitch_vals){
  ## TODO: I'm pretty sure this can be rewritten like a binary search,
  ##       which should be faster than the current implementation. This
  ##       could also be easily rewritten in C++ for further speedup
  stopifnot(new_times[1] >= old_times[1])
  stopifnot(new_times[length(new_times)] <= old_times[length(old_times)])
  stopifnot(length(old_times) == length(pitch_vals))


  left_times <- new_times[NA]
  right_times <- new_times[NA]
  left_points <- new_times[NA]
  right_points <- new_times[NA]
  interpolated_values <- new_times[NA]

  j <- 1
  max_j <- length(old_times)
  for (i in seq_along(new_times)) {
    x <- new_times[i]
    difference <- 1

    # If we reach a crossover point, lookup the adjacent values
    while (difference > 0) {
      j <-  min(j + 1, max_j)
      difference <- x - old_times[j]
    }
    left_times[i] <- old_times[j - 1]
    right_times[i] <- old_times[j]
    left_points[i] <- pitch_vals[j - 1]
    right_points[i] <- pitch_vals[j]
    j <- j - 1
  }

  # Inverse weights depending on which point is closer
  left_weights <- abs(new_times - right_times)
  right_weights <- abs(new_times - left_times)

  for (i in seq_along(new_times)) {
    interpolated_values[i] <- stats::weighted.mean(c(left_points[i],
                                                     right_points[i]),
                                                   c(left_weights[i],
                                                     right_weights[i]))
  }

  interpolated_values
}
