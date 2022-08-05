#' Equally-spaced average pitch contours
#'
#' This function averages pitch contours, with the possibility of doing the
#' averaging by section. For example, we could average the prenuclear portion
#' with 15 equally spaced points and then average the nuclear portion with 30
#' equally spaced points. This avoids the problem of setting 45 points for the
#' total utterance but being unsure about whether nuclear points are being
#' averaged with prenuclear points.
#'
#' You can also chain this for higher-order aggregation, e.g. to get the average
#' tune produced by a participant, then the average across all participants:
#' pitchtier_df |>
#' average_pitchtracks(aggregate_by = file ~ utterance + tune + participant) |>
#' average_pitchtracks(aggregate_by = participant ~ utterance + tune)
#'
#' @param raw_pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' and preferably with time normalization already applied
#' @param section_by Column name containing the section designations of the pitch contour
#' @param pulses_per_section An integer vector of how many points to use
#' for each section of the contour. This needs to have EITHER 1 value, which
#' is recycled for all sections OR as many values as there are sections. In the
#' latter case, it is HIGHLY RECOMMENDED that this vector is NAMED and in the
#' desired order of the sections. If the vector is unnamed, it will use the
#' names of the section in the order they appear in the data.
#' @param time_by Quoted column name containing the timepoints, defaults to
#' `"timepoint_norm"` (highly recommended to use `time_normalize` as a
#' preprocessing step beforehand)
#' @param aggregate_by A formula of unquoted column names. The LHS should index
#' individual recordings, e.g. a `file` column. The RHS should index what you
#' want to aggregate over, e.g. by `utterance` and `tune`. This would be written
#' as `file ~ utterance + tune`. Unpredictable results may occur if recordings
#' are not uniquely identifiable or if the LHS is not specified.
#' @param .pitchval Quoted column name containing the pitch values to average
#' over. Defaults to `"hz"`
#'
#' @return A dataframe containing the averaged and equally spaced piecewise
#' pitch contours
#' @export
#'
#' @importFrom dplyr across
average_pitchtracks2 <- function(raw_pitchtier_df,
                                 section_by,
                                 pulses_per_section,
                                 time_by = 'timepoint_norm',
                                 aggregate_by,
                                 .pitchval = 'hz') {
  # TODO: if section by is missing, make a dummy column to hold the sections
  #       and remove it later so it can still be passed to piecewise extract
  stopifnot(section_by %in% names(raw_pitchtier_df))

  # Parse formula, remove any +s, if one sided then use the RHS for both aggregates
  if (length(aggregate_by) == 2L)
    aggregate_by[[3]] <- aggregate_by[[2]]
  aggregate_within <- as.character(aggregate_by[[3L]])
  aggregate_within <- aggregate_within[which(aggregate_within != "+")]
  pulses_by <- as.character(aggregate_by[[2L]])
  pulses_by <- pulses_by[which(pulses_by != '+')]

  # Exctract equal pulses by section
  equal_pulse_df <-
    raw_pitchtier_df |>
    dplyr::group_by(across(all_of(c(pulses_by, aggregate_within)))) |>
    piecewise_interpolate_pulses(section_by,
                                 pulses_per_section,
                                 time_by,
                                 pulses_by,
                                 .pitchval) |>
    dplyr::group_by(across(all_of(c(aggregate_within, "pulse_i", section_by))))

  avg_colname <- paste0("avg_", .pitchval)

  # At each pulse, get the average Hz value and set the time value for the
  # summary pulse value. Crucially, the spacings between average points
  # are equal WITHIN THE SECTION
  suppressMessages(
    dplyr::summarize(
      equal_pulse_df,
      !!sym(avg_colname) := mean(.data[[.pitchval]], na.rm = TRUE),
      !!sym(time_by) := mean(.data[[time_by]])
    )
  )
}
