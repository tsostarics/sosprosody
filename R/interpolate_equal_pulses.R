#' Equally spaced pitch points via nearest neighbor interpolation
#'
#' This function will return equally spaced pitch pulses whose values are obtained
#' via linear interpolation between the adjacent pitch pulses to that time point.
#' For example, consider pitch points at times 1, 2, 3 with values 80, 90, 100.
#' If you want four equally spaced points, they will be at times 1, 1.67, 2.33,
#' and 3. The desired pitch point at 1.67 would be closer to 90 than 80, and so
#' we can use the weighted mean of 80 and 90 with weights corresponding to the swapped
#' distances (weight should be greater for 90, corresponding to the larger distance
#' of 1.67 from 1) to calculate the new pitch value: 86.67
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' and preferably with time normalization already applied
#' @param n_pulses Integer number of pulses to calculate
#' @param time_by Quoted column name containing the timepoints, defaults to
#' `"timepoint_norm"` (highly recommended to use `time_normalize` as a
#' preprocessing step beforehand). Note that the results will have new
#' time-normalized values with the same column name.
#' @param .grouping Quoted column name indexing the unique recordings, defaults
#' to `"file"`
#' @param .pitchval Quoted column name for which pitch values to use, defaults
#' to `"hz"`
#' @param .overridegroups Logical, Whether to override the grouping structure of `pitchtier_df`
#' with what's specified by `file`. Defaults to `FALSE`
#'
#' @return Dataframe with interpolated pulses
#' @export
interpolate_equal_pulses <- function(pitchtier_df,
                                     n_pulses,
                                     time_by = "timepoint_norm",
                                     .pitchval = "hz",
                                     .grouping = 'file',
                                     .overridegroups = FALSE) {
  n_pulses <- as.integer(n_pulses)


  interpolated_df <-
    do.call(rbind, lapply(unique(pitchtier_df[[.grouping]]), \(grp) {

      time_vals <- pitchtier_df[[time_by]][pitchtier_df[[.grouping]] == grp]
      hz_vals <- pitchtier_df[[.pitchval]][pitchtier_df[[.grouping]] == grp]

      pulse_info <-
        setNames(
          list(rep(grp,n_pulses),
               seq(min(time_vals), max(time_vals), length.out = n_pulses)),
          c(.grouping, time_by)
        )

      pulse_info[[.pitchval]] <- interpolate_pitchpoints(pulse_info[[time_by]],
                                                         time_vals,
                                                         hz_vals)
      class(pulse_info) <- 'data.frame'
      attr(pulse_info, "row.names") <- seq_len(n_pulses)
      pulse_info
    } ))

  # If there are duplicates then the interpolation may end up dividing by 0 at some point
  if (anyNA(interpolated_df[[.pitchval]]))
    warning("NA/NaNs detected in output, pitchtier_df$time_by likely has duplicates.")

  interpolated_df
}

.group_by_vec <- function(df, charvec) {
  dplyr::group_by(df, across(all_of(charvec)))
}
