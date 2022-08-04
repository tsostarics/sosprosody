#' Piecewise extraction of equally spaced pitch pulses
#'
#' This is an extension of `extract_equal_pulses` which extracts equal pulses
#' within particular sections. This was written as a component of
#' `average_pitchtracks`, where I needed to extract equally spaced pulses
#' within prenuclear and nuclear regions separately so I could then take
#' averages within the sections. This function can also be useful if you
#' want a lower resolution for one section but a higher resolution for another.
#'
#' @param raw_pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' and preferably with time normalization already applied
#' @param section_by Column name containing the section designations of the pitch contour
#' @param pulses_per_section An integer vector of how many points to use
#' for each section of the contour. This needs to have EITHER 1 value, which
#' is recycled for all sections OR as many values as there are sections. In the
#' latter case, it is HIGHLY RECOMMENDED that this vector is NAMED and in the
#' desired order of the sections. If the vector is unnamed, it will use the
#' names of the section in the order they appear in the data.
#' @param time_by Quoted column name containing the timepoints, defaults to
#' `"timepoint_norm"` (highly recommended to use `time_normalize` as a
#' preprocessing step beforehand). Note that the results will have new
#' time-normalized values with the same column name.
#'
#' @return Dataframe containing equally spaced pulses within separate sections
#' @export
piecewise_extract_pulses <- function(raw_pitchtier_df,
                                     section_by = "is_nuclear",
                                     pulses_per_section,
                                     time_by = 'timepoint_norm') {
  sections <- unique(raw_pitchtier_df[[section_by]]) |> as.character()
  n_specified <- length(pulses_per_section)
  n_sections <- length(sections)

  # Number of pulses should be recyclable or equal to number of
  # sections available in the data given by the grouping column
  stopifnot(n_specified == 1 || n_specified == n_sections)

  # If recyclable, create vector to use
  if (n_specified == 1)
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
  section_split <- split(raw_pitchtier_df, raw_pitchtier_df[[section_by]])

  purrr::map_dfr(sections,
                 \(section) {
                   section_n_pulses <- pulses_per_section[section]

                   # Determine equally spaced timepoints within the section
                   # to place the average points at later
                   timepoint_df <-
                     suppressMessages({
                       section_split[[section]] |>
                         dplyr::summarize(
                           !!sym(time_by) :=
                             seq(min(.data[[time_by]]),
                                 max(.data[[time_by]]),
                                 length.out = section_n_pulses),
                           # Offset the pulse index according to the section
                           pulse_i = seq_len(section_n_pulses) + offsets[section]
                         ) |>
                         dplyr::select('pulse_i', {{time_by}})
                     })
                   section_df <-
                     suppressMessages({
                       section_split[[section]] |>
                         extract_equal_pulses(n_pulses = section_n_pulses) |>
                         dplyr::mutate(pulse_i = .data$pulse_i + offsets[section]) |>
                         # Need to replace the original time values with timepoint_df
                         dplyr::select(-{{time_by}})
                     })

                   joined_df <- suppressMessages(dplyr::left_join(section_df, timepoint_df))
                   joined_df
                 }
  )
}
