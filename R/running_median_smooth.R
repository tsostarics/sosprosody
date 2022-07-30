#' Running median smooth pitch track
#'
#' Given a pitchtier dataframe, smooth the Hz values using a running median
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' @param .k `k` to use for `runmed`
#' @param .hz Quoted variable name to hold new Hz values. Defaults to `hz`,
#' which will override the previous values
#'
#' @return `pitchtier_df` with modified `hz` column or new column specified by
#' `.hz`
#' @export
#'
#' @importFrom stats runmed
running_median_smooth <- function(pitchtier_df, .k = 5, .hz = 'hz') {
  pitchtier_df |>
    dplyr::group_by(file) |>
    dplyr::mutate(!!sym(.hz) := runmed(.data$hz, k = .k))
}
