#' Equally spaced pitch points via nearest neighbor interpolation
#'
#' This function will return equally spaced pitch pulses whose values are
#' obtained via linear interpolation between the adjacent pitch pulses to that
#' time point. For example, consider pitch points at times 1, 2, 3 with values
#' 80, 90, 100. If you want four equally spaced points, they will be at times 1,
#' 1.67, 2.33, and 3. The desired pitch point at 1.67 would be closer to 90 than
#' 80, and so we can use the weighted mean of 80 and 90 with weights
#' corresponding to the swapped distances (weight should be greater for 90,
#' corresponding to the larger distance of 1.67 from 1) to calculate the new
#' pitch value: 86.67
#'
#' @param n_pulses Integer number of pulses to calculate
#' @param .overridegroups Logical, Whether to override the grouping structure of
#'   `pitchtier_df` with what's specified by `file`. Defaults to `FALSE`
#' @inheritParams piecewise_interpolate_pulses
#'
#' @return Dataframe with interpolated pulses
#' @export
interpolate_equal_pulses <- function(pitchtier_df,
                                     n_pulses,
                                     x,
                                     y,
                                     group,
                                     .overridegroups = FALSE,
                                     .grouping,
                                     .pitchval,
                                     time_by) {

  deprecate_argument(x, time_by)
  deprecate_argument(y, .pitchval)
  deprecate_argument(group, .grouping)

  n_pulses <- as.integer(n_pulses)


  interpolated_df <-
    do.call(rbind, lapply(unique(pitchtier_df[[group]]), \(grp) {

      time_vals <- pitchtier_df[[x]][pitchtier_df[[group]] == grp]
      hz_vals <- pitchtier_df[[y]][pitchtier_df[[group]] == grp]

      pulse_info <-
        setNames(
          list(rep(grp,n_pulses),
               seq(min(time_vals), max(time_vals), length.out = n_pulses)),
          c(group, x)
        )

      pulse_info[[y]] <- interpolate_pitchpoints(pulse_info[[x]],
                                                         time_vals,
                                                         hz_vals)
      class(pulse_info) <- 'data.frame'
      attr(pulse_info, "row.names") <- seq_len(n_pulses)
      pulse_info
    } ))

  # If there are duplicates then the interpolation may end up dividing by 0 at some point
  if (anyNA(interpolated_df[[y]]))
    warning("NA/NaNs detected in output, pitchtier_df$x likely has duplicates.")

  interpolated_df
}

.group_by_vec <- function(df, charvec) {
  dplyr::group_by(df, across(all_of(charvec)))
}
