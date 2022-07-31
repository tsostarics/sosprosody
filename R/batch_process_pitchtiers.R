#' Load multiple pitchtiers into wrangled dataframe
#'
#' Wrapper around filepath |> pitchtier_to_dataframe()
#' merged together via `purrr::map_dfr`
#'
#' @param ptdir Directory containing pitchtier files
#' @param parallelize Logical, whether to run in parallel with `furrr::future_map_dfr`
#'
#' @return Dataframe of the pitchtier information from each file
#' @export
batch_process_pitchtiers <- function(ptdir, parallelize = FALSE) {
  pt_files <- paste0(ptdir, list.files(ptdir,pattern = ".PitchTier$",ignore.case = TRUE))

  map_fx <- purrr::map_dfr

  if (parallelize) {
    requireNamespace("furrr", quietly = TRUE)
    map_fx <- furrr::future_map_dfr
  }

  dat <-
    map_fx(
      pt_files,
      \(filepath)
        pitchtier_to_dataframe(
          rPraat::pt.read(filepath),
          .quiet = TRUE
        )

    )

  message(glue::glue("Processed {length(pt_files)} PitchTiers"))

  dat
}
