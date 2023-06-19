.formatTier <- function(x, ..., tier_var = 'f', .horiz_res = 80L, .vert_res = 10L) {
  # Establish minimum dimensions for printing pitchtier objects to avoid errors
  if (.vert_res < 3)
    .vert_res <- 4
  if (.vert_res %% 2)
    .vert_res <- .vert_res + 1
  if (.horiz_res < 10)
    .horiz_res <- 10

  # Get y-axis labels in Hz, then readjust the horizontal resolution
  # to accomodate the labels
  fmin <- as.character(round(min(x[[tier_var]]), 0))
  fmax <- as.character(round(max(x[[tier_var]]), 0))
  yaxislen <- max(nchar(c(fmin, fmax)))
  .horiz_res <- .horiz_res - yaxislen
  # Pad the Hz values with leading spaces as needed
  fmin <- paste0(rep(" ", times = yaxislen - nchar(fmin)), fmin)
  fmax <- paste0(rep(" ", times = yaxislen - nchar(fmax)), fmax)

  # Get the total duration of the pitchtier object
  tmin <- round(x[['tmin']], 2)
  tmax <- round(x[['tmax']], 2)

  # Establish the domain for the pitch pulses (!= total duration)
  first_pulse <- x[['t']][1]
  last_pulse <- x[['t']][length(x[['t']])]
  first_frame <- round((first_pulse * .horiz_res) / tmax, 0)
  last_frame <- round((last_pulse * .horiz_res) / tmax, 0)
  n_pulses <- last_frame - first_frame

  # Get evenly spaces pulses to fit the pitch pulse domain
  pitch_vals <- interpolate_pitchpoints(new_times = seq(tmin,
                                                        tmax,
                                                        length.out = n_pulses),
                                        old_times = x[['t']],
                                        pitch_vals = x[[tier_var]] ) - mean(x[[tier_var]])

  # Establish pitch levels from the vertical resolution
  min_st <- min(pitch_vals)
  max_st <- max(pitch_vals)
  pitch_levels <-
    c(seq(min_st, 0, length.out = round(.vert_res/2 - .5, 0)),
      seq(0, max_st, length.out = round(.vert_res/2 + .5, 0))[-1])

  # Map the semitone values to the closes pitch level
  y_vals <- vapply(pitch_vals,
                   \(x)
                   which.min(abs(x - pitch_levels)),
                   1.0)

  # Allocate matrix to fill with the plot
  holder <- matrix(" ", ncol = .horiz_res, nrow = .vert_res)

  # Add the pitch pulses to the plot
  pulse_train <- seq(first_frame, last_frame)
  for (x_i in seq_len(n_pulses)) {
    holder[y_vals[x_i],pulse_train[x_i]] <- "O"
  }

  # Determine how much space to pad between the start and end timepoints
  tmin <- as.character(tmin)
  tmax <- as.character(tmax)
  padding <- .horiz_res - (nchar(tmin) + nchar(tmax))
  xaxis <- paste0(paste0(rep(" ", times = yaxislen), collapse = ""),
                  tmin,
                  paste0(rep(" ", times = padding), collapse = ""),
                  tmax,
                  collapse = "")
  # Add vertical bounding lines on both sides
  holder[,c(1,.horiz_res)] <- "|"

  # Add the y-axis labels
  holder <- cbind(c(fmin,
                    rep(paste0(rep(" ", yaxislen),collapse=""), .vert_res - 2) ,
                    fmax),
                  holder)

  # Print the plot, indices are descending so we need to reverse
  # note: using paste0 w collapse is faster than using cat's sep
  header <- paste0(class(x)[['name']],
                   ": ",
                   length(x[[tier_var]]),
                   " total pulses.\n",
                   collapse = "")
  pitchplot <-
    vapply(rev(seq_len(.vert_res)),
           \(y_i)
           paste0(holder[y_i,], collapse = ""),
           "char") |>
    paste0(collapse = "\n")

  paste0(header, pitchplot,"\n", xaxis)
}
