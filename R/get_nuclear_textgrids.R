#' Get nuclear textgrid region
#'
#' Given a dataframe of textgrid information from `batch_process_textgrids`,
#' filter to only include the region corresponding to the nuclear words
#' specified by `nuclear_words`. This implicitly assumes that each target
#' utterance has a unique nuclear word.
#'
#' @param textgrid_df Output of `batch_process_textgrids`
#' @param nuclear_words Character vector of nuclear words to use
#' @param .first Whether to only include the first result, default TRUE
#'
#' @return Filtered dataframe with the nuclear region information for each file
#' @export
get_nuclear_textgrids <- function(textgrid_df,
                                  nuclear_words,
                                  .first = TRUE) {
  if (missing(nuclear_words)) {
    stopifnot('is_nuclear' %in% names(textgrid_df))
  } else {
    textgrid_df <- dplyr::mutate(textgrid_df,
                                 is_nuclear = .data$word_label %in% nuclear_words)
  }

  n_files <- length(unique(textgrid_df[['file']]))
  textgrid_df <- dplyr::filter(textgrid_df, .data$is_nuclear)
  n_files_now <- length(unique(textgrid_df[['file']]))

  if (n_files != n_files_now)
    warning(glue::glue("Filtered textgrid contains {n_files-n_files_now} fewer files, did you forget a few nuclear words?"))

  if (.first)
    textgrid_df <-
    textgrid_df |>
    dplyr::group_by(file) |>
    dplyr::summarize(dplyr::across(tidyselect::everything(), dplyr::first))

  textgrid_df
}
