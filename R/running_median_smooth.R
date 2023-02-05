#' Running median smooth pitch track
#'
#' Given a pitchtier dataframe, smooth the Hz values using a running median
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' @param .k `k` to use for `runmed`
#' @param .from Quoted variable name to pull Hz values from. Defaults to `.hz`
#' @param .to Quoted variable name to put smoothed Hz values. Defaults to
#' `runmed_hz`. If this is the same as `.from`, the values will be overwritten.
#' @param .grouping Character vector of columns to group by, defaults to `'file'`
#'
#' @return `pitchtier_df` with modified `hz` column or new column specified by
#' `.hz`
#' @export
#'
#' @importFrom stats runmed
#' @importFrom tidyselect all_of
running_median_smooth <- function(pitchtier_df,
                                  .k = 5,
                                  .from = 'hz',
                                  .to = 'runmed_hz',
                                  .grouping = "file") {
  pitchtier_df |>
    .group_by_vec(.grouping) |>
    dplyr::mutate(!!sym(.to) := runmed(!!sym(.from), k = .k))
}
