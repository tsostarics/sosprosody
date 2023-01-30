#' Time normalize pitch track
#'
#' Normalizes the timepoints to be out of 1. If `.fromzero` is `TRUE`, all
#' values will be subtracted by the first timepoint, making the domain `[0,1]`.
#' If `.fromzero` is `FALSE`, the domain will be `(0,1]`.
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' @param .from Quoted variable name to pull timepoint values from. Defaults to
#' `timepoint`
#' @param .to Quoted variable name to hold new timepoint values.
#' Defaults to `timepoint_norm`. If this is the same as `.from`, the values will
#' be overwritten.
#' @param .fromzero Logical, whether to shift timepoints to start from 0, defaults
#' to TRUE.
#' @param .grouping Quoted column name indexing the unique recordings, defaults
#' to `"file"`
#' @param .overridegroups Logical, Whether to override the grouping structure of `pitchtier_df`
#' with what's specified by `file`. Defaults to `FALSE`
#'
#' @return `pitchtier_df` with modified `timepoint` column or new column
#' specified by `.timepoint`
#' @export
time_normalize <- function(pitchtier_df,
                           .from = 'timepoint',
                           .to = "timepoint_norm",
                           .fromzero = TRUE,
                           .grouping = 'file',
                           .overridegroups = FALSE) {
  if (!.from %in% names(pitchtier_df))
    stop(glue::glue("Column `{.from}` not found in pitchtier_df"))

  full_groupings <- .get_groupings(pitchtier_df, .grouping, .overridegroups)
  pitchtier_df <- dplyr::group_by(pitchtier_df, across(all_of(full_groupings)))

  if (.fromzero)
    pitchtier_df <- dplyr::mutate(pitchtier_df, !!sym(.from) := !!sym(.from) - min(!!sym(.from)))

  pitchtier_df |>
    dplyr::mutate(!!sym(.to) := !!sym(.from) / max(!!sym(.from)))
}
