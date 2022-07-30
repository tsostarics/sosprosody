#' Load multiple pitchtiers into wrangled dataframe
#'
#' Wrapper around filepath |> pitchtier_to_dataframe()  |> separate_fileinfo(),
#' merged together via `purrr::map_dfr`
#'
#' @param ptdir Directory containing pitchtier files
#'
#' @return Dataframe of the pitchtier information from each file
batch_process_pitchtiers <- function(ptdir) {
  pt_files <- paste0(ptdir, list.files(ptdir,pattern = ".PitchTier$",ignore.case = TRUE))
  dat <-
    purrr::map_dfr(
      pt_files,
      \(filepath)
      separate_fileinfo(
        pitchtier_to_dataframe(
          rPraat::pt.read(filepath),
          .quiet = TRUE
        )
      )
    )

  message(glue::glue("Processed {length(pt_files)} PitchTiers"))

  dat
}
