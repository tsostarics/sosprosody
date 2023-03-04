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
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
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
#' are not uniquely identifiable by the column specified in the LHS.
#' @param .pitchval Quoted column name containing the pitch values to average
#' over. Defaults to `"hz"`
#' @param parallelize Whether to run in parallel via multisession
#' `furrr::future_map_dfr`, passed on to `piecewise_interpolate_pulses`
#' @param index_column See `piecewise_interpolate_pulses`
#'
#' @return A dataframe containing the averaged and equally spaced piecewise
#' pitch contours
#' @export
#'
#' @importFrom dplyr across
#' @importFrom stats terms
average_pitchtracks <- function(pitchtier_df,
                                section_by,
                                pulses_per_section,
                                time_by = 'timepoint_norm',
                                aggregate_by,
                                .pitchval = 'hz',
                                index_column = NULL,
                                parallelize = FALSE) {

  stopifnot(section_by %in% names(pitchtier_df))

  if (length(aggregate_by) != 3)
    stop("Formula for aggregate_by must be two-sided")
  if (length(aggregate_by[[2]]) > 1)
    stop("LHS must contain only one column identifying unique columns")

  pulses_by <- all.vars(aggregate_by)[1L] # LHS term
  aggregate_within <- labels(terms(aggregate_by)) # RHS terms

  # Exctract equal pulses by section
  equal_pulse_df <-
    pitchtier_df |>
    .group_by_vec(c(pulses_by, aggregate_within)) |>
    piecewise_interpolate_pulses(section_by = section_by,
                                 pulses_per_section = pulses_per_section,
                                 time_by = time_by,
                                 index_column = index_column,
                                 .grouping = pulses_by,
                                 .pitchval = .pitchval,
                                 parallelize = parallelize)

  groupings <- c(aggregate_within, "pulse_i", section_by)

  data.table::setDT(equal_pulse_df)
  avg_colname <- paste0("avg_", .pitchval)

  averaged_df <-
    equal_pulse_df[, stats::setNames(list(mean(get(.pitchval)),
                                          mean(get(time_by))),
                                     c(avg_colname, time_by)),
                 by = groupings]

  .group_by_vec(averaged_df, groupings)

}
