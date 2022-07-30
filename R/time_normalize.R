#' Time normalize pitch track
#'
#' Changes the timepoints to be between (0, 1]
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' @param .timepoint Quoted variable name to hold new timepoint values.
#' Defaults to `timepoint`, which will override the previous values.
#'
#' @return `pitchtier_df` with modified `timepoint` column or new column
#' specified by `.timepoint`
#' @export
time_normalize <- function(pitchtier_df, .timepoint = 'timepoint') {
  pitchtier_df |>
    dplyr::group_by(file) |>
    dplyr::mutate(!!sym(.timepoint) := .data$timepoint / max(.data$timepoint))
}
