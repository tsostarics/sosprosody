#' Code pulses by nuclear status
#'
#' Given a pitchtier dataframe and a correponding nuclear region dataframe
#' from `get_nuclear_tg`, code each pulse for each file on whether it lies
#' within the nuclear region or not
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtier`
#' @param nuclear_df Nucler region dataframe, output of `get_nuclear_tg`
#'
#' @return `pitchtier_df` but with a new logical column `is_nuclear`
#' @export
#'
#' @importFrom rlang .data
code_nuclear_pulses <- function(pitchtier_df,
                                nuclear_df) {
  stopifnot(is.data.frame(nuclear_df))

  pitchtier_df |>
    dplyr::group_by(file) |>
    dplyr::mutate(is_nuclear =
                    dplyr::between(
                      .data$timepoint,
                      .get_time(nuclear_df, file, 'word_start'),
                      .get_time(nuclear_df, file, 'word_end')
                    )
    )
}

#' Get word boundary time
#'
#' Helper for `code_nuclear_pulses` to lookup nuclear word start and end
#' timestamps. This is called from within a `mutate` call.
#'
#' @param nuclear_regions Output of `get_nuclear_tg`
#' @param files Files, from within the mutate call on a grouped data frame,
#' will grab the first file name
#' @param .which Either `word_start` or `word_end`
#'
#' @return Timestamp specified by `.which`
.get_time <- function(nuclear_regions, files, .which = 'word_start') {
  nuclear_regions[nuclear_regions[['file']] == dplyr::first(files), .which][[1]]
}
