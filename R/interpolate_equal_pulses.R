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
  full_groupings <- .grouping #.get_groupings(pitchtier_df, .grouping, .overridegroups)

  reframe_fx <- getOption("sosprosody.reframe_fx")

  time_range <- range(pitchtier_df[[time_by]])

  interpolated_df <-
    data.frame(f = pitchtier_df[[.grouping]][1],
               t = seq(time_range[1],
                       time_range[2],
                       length.out = n_pulses))
  colnames(interpolated_df) <- c(.grouping, time_by)
  # Set up the timestamps of the new pulses to interpolate
  # interpolated_df <-
  #   pitchtier_df |>
  #   # .group_by_vec(full_groupings) |>
  #   # dplyr::summarize(min_time = min(.data[[time_by]]),
  #   #                  max_time = max(.data[[time_by]]),
  #   #                  .groups = "keep") |>
  #   reframe_fx(!!sym(time_by) := seq(.data[['min_time']],
  #                                          .data[['max_time']],
  #                                          length.out = n_pulses))

  # Interpolate n_pulses between first and last timepoints
  group_vals <- unique(pitchtier_df[[.grouping]])

  interpolated_df[[.pitchval]] <- interpolate_pitchpoints(interpolated_df[[time_by]],
                                                          pitchtier_df[[time_by]],
                                                          pitchtier_df[[.pitchval]])

  # result <-
  #   purrr::map_dfr(group_vals,
  #                  \(grp) {
  #                    # Get only the current group timestamps and pitch vals
  #                    int_df <- interpolated_df[interpolated_df[[.grouping]] == grp,]
  #                    pt_df <- pitchtier_df[pitchtier_df[[.grouping]] == grp,]
  #
  #                    # Calculate the interpolated pitch values, add to interpolated df
  #                    int_df[[.pitchval]] <- interpolate_pitchpoints(int_df[[time_by]],
  #                                                                   pt_df[[time_by]],
  #                                                                   pt_df[[.pitchval]])
  #                    int_df
  #                  })

  # If there are duplicates then the interpolation may end up dividing by 0 at some point
  if (anyNA(interpolated_df[[.pitchval]]))
    warning("NA/NaNs detected in output, pitchtier_df$time_by likely has duplicates.")

  interpolated_df
}

.group_by_vec <- function(df, charvec) {
  dplyr::group_by(df, across(all_of(charvec)))
}
