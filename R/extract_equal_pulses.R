#' Extract equally spaced pulses
#'
#' Given a pitchtier dataframe with a `file` column, extract a number of
#' equally spaced pulses.
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' @param n_pulses Number of pulses to extract, will be coerced to an integer.
#' Note that each file must have at least this many pulses.
#'
#' @return Filtered dataframe containing equally spaced pulses. The original
#' pulse index is added as the `pulse_i` column.
#' @export
extract_equal_pulses <- function(pitchtier_df, n_pulses = 30) {
  n_pulses <- as.integer(n_pulses)

  # Determine how many pulses each file has
  pulse_counts <-
    pitchtier_df |>
    dplyr::group_by(file) |>
    dplyr::summarize(n_samples = dplyr::n())

  # Ensure that every file has at least n_pulses pulses
  if (any(pulse_counts[['n_samples']] < n_pulses)) {
    min_index <- which.min(pulse_counts[['n_samples']])
    min_file <- pulse_counts[['file']][min_index]
    min_count <- pulse_counts[['n_samples']][min_index]
    stop(glue::glue("{n_pulses} pulses requested but {min_file} only has {min_count}"))
  }

  # Determine the indices to extract for each file
  pulse_indices <-
    lapply(pulse_counts[['n_samples']],
           \(x)
           round(seq(1, x, length.out = n_pulses), 0))
  names(pulse_indices) <- pulse_counts[['file']]

  # Extract the calculated pulses
  pitchtier_df |>
    dplyr::group_by(file) |>
    dplyr::mutate(pulse_i = dplyr::row_number(),
                  keep_pulse = .data$pulse_i %in% pulse_indices[[dplyr::first(file)]]) |>
    dplyr::filter(.data$keep_pulse) |>
    dplyr::select(-'keep_pulse')
}
