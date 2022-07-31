#' Load multiple textgrids into wrangled dataframe
#'
#' Wrapper around filepath |> textgrid_to_dataframes() |> nest_tiers()
#' merged together via `purrr::map_dfr`
#'
#' Note: The textgrids are assumed (for now) to only contain 2 interval tiers:
#' `words` and `phones`, as given by the output of the Montreal forced aligner.
#'
#' @param tgdir Directory containing textgrids
#' @param parallelize Logical, whether to run in parallel with `furrr::future_map_dfr`
#'
#' @return Dataframe of the word and phone information from each textgrid
#' @export
batch_process_textgrids <- function(tgdir, parallelize = TRUE) {
  tg_files <- paste0(tgdir, list.files(tgdir,pattern = ".TextGrid$",ignore.case = TRUE))
  map_fx <- purrr::map_dfr

  if (parallelize) {
    requireNamespace("furrr", quietly = TRUE)
    map_fx <- furrr::future_map_dfr
  }

  dat <-
    map_fx(
      tg_files,
      \(filepath)
      nest_tiers(
        textgrid_to_dataframes(
          rPraat::tg.read(filepath)
        )
      )

    )

  message(glue::glue("Processed {length(tg_files)} TextGrids"))

  dat
}
