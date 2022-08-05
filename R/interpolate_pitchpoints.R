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

    while (difference > 0) {
      j <-  min(j + 1, max_j)
      difference <- x - old_times[j]
    }
    left_times[i] <- old_times[j - 1]
    right_times[i] <- old_times[j]
    left_points[i] <- pitch_vals[j - 1]
    right_points[i] <- pitch_vals[j]
  }

  # Inverse weights depending on which point is closer
  left_weights <- abs(new_times - right_times)
  right_weights <- abs(new_times - left_times)

  for (i in seq_along(new_times)) {
    interpolated_values[i] <- weighted.mean(c(left_points[i],
                                              right_points[i]),
                                            c(left_weights[i],
                                              right_weights[i]))
  }

  interpolated_values
}
