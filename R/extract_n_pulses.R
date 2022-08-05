#' Extract equally spaced pulses
#'
#' Given a pitchtier dataframe with a `file` column, extract a number of
#' equally spaced pulses.
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' @param n_pulses Number of pulses to extract, will be coerced to an integer.
#' Note that each file must have at least this many pulses.
#' @param .keep_indices Whether to keep original pulse indices along with the
#' new indices. If averaging over pulses later, be sure to use `pulse_i`
#'
#' @return Filtered dataframe containing equally spaced pulses. The original
#' pulse index is added as the `pulse_i` column.
#' @export
#'
#' @examples
#'
#' extract_n_pulses(data.frame(file = 'a', hz = 9:18), n_pulses = 3, .keep_indices = TRUE)
extract_n_pulses <- function(pitchtier_df,
                                 n_pulses = 30,
                                 .keep_indices = FALSE,
                                 .grouping = "file") {
  n_pulses <- as.integer(n_pulses)

  # Determine how many pulses each file has
  pulse_counts <-
    pitchtier_df |>
    dplyr::group_by(across(all_of(.grouping))) |>
    dplyr::summarize(n_samples = dplyr::n())

  # Ensure that every file has at least n_pulses pulses
  if (any(pulse_counts[['n_samples']] < n_pulses)) {
    min_index <- which.min(pulse_counts[['n_samples']])
    min_file <- pulse_counts[[.grouping]][min_index]
    min_count <- pulse_counts[['n_samples']][min_index]
    stop(glue::glue("{n_pulses} pulses requested but {min_file} only has {min_count}"))
  }

  # Determine the indices to extract for each file
  pulse_indices <-
    lapply(pulse_counts[['n_samples']],
           \(x)
           round(seq(1, x, length.out = n_pulses), 0))
  names(pulse_indices) <- pulse_counts[[.grouping]]

  # Extract the calculated pulses
  pitchtier_df <-
    pitchtier_df |>
    dplyr::group_by(across(all_of(.grouping))) |>
    dplyr::mutate(pulse_i = dplyr::row_number(),
                  keep_pulse = .data$pulse_i %in% pulse_indices[[dplyr::first(.data[[.grouping]])]]) |>
    dplyr::filter(.data$keep_pulse) |>
    dplyr::select(-'keep_pulse')

  if (.keep_indices)
    pitchtier_df[["old_pulse_i"]] <- pitchtier_df[['pulse_i']]

  dplyr::mutate(pitchtier_df, pulse_i = dplyr::row_number())

}
