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
#'
#' ```
#' pitchtier_df |>
#' average_pitchtracks(aggregate_by = file ~ utterance + tune + participant) |>
#' average_pitchtracks(aggregate_by = participant ~ utterance + tune)
#' ```
#'
#' @inheritParams piecewise_interpolate_pulses
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' and preferably with time normalization already applied
#' @param aggregate_by A formula of unquoted column names. The LHS should index
#' individual recordings, e.g. a `file` column. The RHS should index what you
#' want to aggregate over, e.g. by `utterance` and `tune`. This would be written
#' as `file ~ utterance + tune`. Unpredictable results may occur if recordings
#' are not uniquely identifiable by the column specified in the LHS.
#' `furrr::future_map_dfr`, passed on to `piecewise_interpolate_pulses`
#'
#' @return A dataframe containing the averaged and equally spaced piecewise
#' pitch contours
#' @export
#'
#' @importFrom dplyr across
#' @importFrom stats terms
average_pitchtracks <- function(pitchtier_df,
                                section_by = NULL,
                                pulses_per_section,
                                x,
                                aggregate_by,
                                y,
                                group,
                                index_by = NULL,
                                sort_first = FALSE,
                                parallelize = FALSE,
                                .grouping,
                                .sort,
                                .pitchval,
                                time_by,
                                index_column) {
  deprecate_argument(x, time_by)
  deprecate_argument(y, .pitchval)
  deprecate_argument(group, .grouping)
  deprecate_argument(index_by, index_column)
  deprecate_argument(sort_first, .sort)

  if (length(aggregate_by) != 3)
    stop("Formula for aggregate_by must be two-sided")
  if (length(aggregate_by[[2]]) > 1)
    stop("LHS must contain only one column identifying unique columns")

  pulses_by <- all.vars(aggregate_by)[1L] # LHS term
  aggregate_within <- labels(terms(aggregate_by)) # RHS terms

  if (is.null(section_by))
    section_by <- pulses_by
  stopifnot(section_by %in% names(pitchtier_df))

  # Exctract equal pulses by section
  equal_pulse_df <-
    pitchtier_df |>
    .group_by_vec(c(pulses_by, aggregate_within)) |>
    piecewise_interpolate_pulses(section_by = section_by,
                                 pulses_per_section = pulses_per_section,
                                 x = x,
                                 index_by = index_by,
                                 group = pulses_by,
                                 y = y,
                                 sort_first = sort_first,
                                 parallelize = parallelize)

  groupings <- c(aggregate_within, "pulse_i", section_by)

  data.table::setDT(equal_pulse_df)
  avg_colname <- paste0("avg_", y)

  averaged_df <-
    equal_pulse_df[, stats::setNames(list(mean(get(y)),
                                          mean(get(x))),
                                     c(avg_colname, x)),
                 by = groupings]

  averaged_df
}
