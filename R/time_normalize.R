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
#'
#' @return `pitchtier_df` with modified `timepoint` column or new column
#' specified by `.timepoint`
#' @export
time_normalize <- function(pitchtier_df,
                           .from = 'timepoint',
                           .to = "timepoint_norm",
                           .fromzero = TRUE) {
  if (!.from %in% names(pitchtier_df))
    stop(glue::glue("Column `{.from}` not found in pitchtier_df"))

  if (.fromzero)
    pitchtier_df[[.from]] <- pitchtier_df[[.from]] - min(pitchtier_df[[.from]])

  pitchtier_df |>
    dplyr::group_by(file) |>
    dplyr::mutate(!!sym(.to) := !!sym(.from) / max(!!sym(.from)))
}
