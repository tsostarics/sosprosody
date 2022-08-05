piecewise_interpolate_pulses <- function(pitchtier_df,
                                         section_by = "is_nuclear",
                                         pulses_per_section,
                                         time_by = 'timepoint_norm',
                                         .grouping = 'file',
                                         .pitchval = 'hz') {
  sections <- unique(pitchtier_df[[section_by]]) |> as.character()
  n_specified <- length(pulses_per_section)
  n_sections <- length(sections)

  # Number of pulses should be recyclable or equal to number of
  # sections available in the data given by the grouping column
  stopifnot(n_specified == 1 || n_specified == n_sections)

  # If recyclable, create vector to us
  pulses_per_section <- rep(pulses_per_section, n_sections)

  # If names were not provided, or n pulses was recyclable, then the
  # vector will have no names, so we'll set to the section names
  if (is.null(names(pulses_per_section)))
    names(pulses_per_section) <- sections

  # If names were provided in the pulses_per_section vector,
  # ensure that all the names are actually in the grouping column
  stopifnot(all(names(pulses_per_section) %in% sections))

  # Offsets for the pulse indices based on the section they're in
  # note that this is why the names of pulses_per_section must be
  # given in the desired order
  offsets <- c(0, cumsum(pulses_per_section)[-n_sections])
  names(offsets) <- sections

  purrr::map_dfr(sections,
                 \(section) {
                   section_n_pulses <- pulses_per_section[section]

                   interpolated_df <-
                     interpolate_equal_pulses(pitchtier_df[pitchtier_df[[section_by]]==section,],
                                              n_pulses = section_n_pulses,
                                              .pitchval = .pitchval,
                                              .grouping = .grouping) |>
                     dplyr::mutate(pulse_i = seq_len(section_n_pulses) + offsets[section],
                                   !!sym(section_by) := section)
                 }
  )
}
