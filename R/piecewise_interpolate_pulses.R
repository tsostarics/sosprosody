#' Piecewise interpolation of equally spaced pitch pulses
#'
#' This is an extension of `interpolate_equal_pulses` which extracts equal pulses
#' within particular sections of a time series. This was written as a component of
#' `average_pitchtracks`, where I needed to extract equally spaced pulses
#' within prenuclear and nuclear regions separately so I could then take
#' averages within the sections. This function can also be useful if you
#' want a lower resolution for one section but a higher resolution for another.
#'
#' Note that if you have two sections that share a boundary, you will get
#' two pulses at the same timepoint-- one associated with the end of the first
#' section, the other at the start of the second. If you plot the pulse indices
#' you'll encounter cases where there's no change from one pulse to another
#' at these shared timestamps. If you want to remove one of them, please refer
#' to `adjust_pulses()`.
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' and preferably with time normalization already applied
#' @param section_by Column name containing the section designations of the pitch contour
#' @param pulses_per_section An integer vector of how many points to use
#' for each section of the contour. This needs to have EITHER 1 value, which
#' is recycled for all sections OR as many values as there are sections. In the
#' latter case, it is HIGHLY RECOMMENDED that this vector is NAMED and in the
#' desired order of the sections. If the vector is unnamed, it will use the
#' names of the section in the order they appear in the data.
#' @param index_column String, column name containing numeric indices of each
#' interval to extract pulses from. This really needs to be provided if you have
#' two adjacent intervals that have the same label in the column `section_by`.
#' Without unique indices differentiating them, this function will treat them
#' as one "long" interval. If and only if you do not have this case present in
#' your data, then you do not need to provide this. Leave it as NULL if
#' you don't have the indices in your dataframe & the function will try to
#' assign numeric indices based on when the labels in `section_by` change. If
#' you do have them, but don't have the case previously described with adjacent
#' labeled intervals, still provide the column name to save computation time.
#' @param time_by Quoted column name containing the timepoints, defaults to
#' `"timepoint_norm"` (highly recommended to use `time_normalize` as a
#' preprocessing step beforehand). Note that the results will have new
#' time-normalized values with the same column name.
#' @param .grouping Quoted column name indexing the unique recordings, defaults
#' to `"file"`
#' @param .pitchval Quoted column name for which pitch values to use, defaults
#' to `"hz"`
#' @param parallelize Logical, defaults to FALSE, whether to run in parallel via multisession
#' `furrr::future_map_dfr`
#' @param .sort Logical, defaults to TRUE, whether to sort the dataframe
#' by the values in `time_by` for each group specified by `.grouping`. If your
#' dataframe is large, you should consider pre-sorting it before passing to this
#' function & set this to FALSE. The sorting is needed to ensure that the pulse
#' indices are in the correct order.
#'
#' @return Dataframe of interpolated pitch pulses by section
#' @export
piecewise_interpolate_pulses <- function(pitchtier_df,
                                         section_by = "is_nuclear",
                                         pulses_per_section,
                                         index_column = NULL,
                                         time_by = 'timepoint_norm',
                                         .grouping = 'file',
                                         .pitchval = 'hz',
                                         .sort = TRUE,
                                         parallelize = FALSE) {
  DROP_INDEX <- FALSE
  pitchtier_df_cols <- colnames(pitchtier_df)
  stopifnot(section_by %in% pitchtier_df_cols)
  stopifnot(time_by %in% pitchtier_df_cols)
  full_groupings <- .get_groupings(pitchtier_df, .grouping)

  pitchtier_df <-
    pitchtier_df |>
    .group_by_vec(.grouping)

  # Ensure timepoints are ordered (if not sorted the pulse indices may be wrong)
  if (.sort)
    pitchtier_df <-
    pitchtier_df |>
    dplyr::arrange(.data[[time_by]], .data[[section_by]], .by_group = TRUE)


  # Get unique indices for each interval, guess if not provided
  if (is.null(index_column)) {

    pitchtier_df <- pitchtier_df |>
      dplyr::mutate(sosprosody_interval_i  = .guess_interval_indices(.data[[section_by]]))

    # interval_indices <- .guess_interval_indices(pitchtier_df[[section_by]])
    index_column <- 'sosprosody_interval_i'
    # pitchtier_df[[index_column]] <- interval_indices
    DROP_INDEX <- TRUE
  } else {
    stopifnot(is.character(index_column) & (length(index_column) == 1))

    stopifnot(index_column %in% pitchtier_df_cols)
    interval_indices <- pitchtier_df[[index_column]]

    stopifnot(is.numeric(interval_indices))
  }

  # Use unique intervals and section names to set appropriate # of pulses
  unique_sections <- as.character(unique(pitchtier_df[[section_by]]))
  pulses_per_section <- .fill_pps(pulses_per_section, unique_sections)


  map_method <- purrr::map_dfr
  if (parallelize) {
    future::plan("multisession")
    map_method <- furrr::future_map_dfr
  }

  pitchtier_df |>
    dplyr::group_split() |>
    purrr::map_dfr(\(file_df) {

      offsets <- .compute_offsets(pulses_per_section,
                                  file_df[[section_by]],
                                  file_df[[index_column]])

      file_df |>
        .group_by_vec(index_column) |>
        dplyr::group_split() |>
        map_method( \(section_df) {
          section_df <- .group_by_vec(section_df, full_groupings)
          interval_idx <- section_df[[index_column]][1L]
          section_label <- section_df[[section_by]][1]
          section_n_pulses <- pulses_per_section[section_label]
          offset <- offsets[as.character(interval_idx)]

          int_df <- interpolate_equal_pulses(section_df,
                                             n_pulses = section_n_pulses,
                                             time_by = time_by,
                                             .pitchval = .pitchval,
                                             .grouping = .grouping) |>
            dplyr::mutate(pulse_i = seq_len(section_n_pulses) + offset,
                          !!sym(section_by) := section_label)

          if (!DROP_INDEX)
            int_df[[index_column]] <- interval_idx

          int_df
        })
    })
}

.fill_pps <- function(pulses_per_section, unique_sections) {
  # Temp value to recycle later
  recycled_pulse_value <- 10

  # Get the names from pulses_per_section and get the number of unnamed elements
  pps_names <- names(pulses_per_section)
  is_unnamed_value <- pps_names == ""
  n_empty <- sum(is_unnamed_value)

  is_section_name <- pps_names[!is_unnamed_value] %in% unique_sections

  # If no recyclable value is set
  if (n_empty == 0) {
    # # of sections in pulses_per_sections must equal # of unique sections
    if (length(pps_names) != length(unique_sections)) {
      sections_not_set <- paste0(unique_sections[!unique_sections %in% pps_names], collapse = ", ")
      stop("Section not specified in pulses_per_section but no recyclable value is set: {sections_not_set}")
    }

    # Names of pulses_per_sections must all exist in unique sections
    if (!all(is_section_name)) {
      not_existing_sections <- paste0(pps_names[!is_section_name], collapse = ', ')
      stop(glue::glue("Section names not found: {not_existing_sections}"))
    }

    return(pulses_per_section)
  }

  # Only one recyclable value should be set
  if (n_empty > 1)
    stop(glue::glue("pulses_per_section has {n_empty} unnamed values, must provide only 1 to recycle"))

  # Overwrite the temp recycle value to the user-set one, if one was provided
  if (n_empty == 1)
    recycled_pulse_value <- pulses_per_section[is_unnamed_value]

  # Temporarily set all sections to the recycled value
  section_pulses <- rep(recycled_pulse_value, length(unique_sections))
  names(section_pulses) <- unique_sections

  # Now set the user-specified pulse numbers to each section
  for (pps_name in pps_names[!is_unnamed_value]) {
    section_pulses[pps_name] <- pulses_per_section[pps_name]
  }

  section_pulses
}

.guess_interval_indices <- function(section_column) {
  indices <- section_column[NA]
  interval_idx <- 1L
  i <- 1L

  previous_section <- section_column[1L]

  while (i <= length(section_column)) {
    current_section <- section_column[i]

    if (current_section != previous_section) {
      interval_idx <- interval_idx + 1L
      previous_section <- current_section
    }

    indices[i] <- interval_idx
    i <- i + 1L
  }

  indices
}

.compute_offsets <- function(pulses_per_section, section_column, interval_indices) {
  unique_indices <- unique(interval_indices)
  n_sections <- length(unique_indices)

  # Allocate a vector mapping the indices to the section labels
  section_mapping <- rep(NA_character_, n_sections)

  # Given |interval_indices| = K, where interval_indices contains k unique
  # values s.t. |k| <= K and |k| = n_sections. Assign each value k to its
  # corresponding section given in section_column

  section_idx <- 1L
  previous_interval <- interval_indices[1L]
  section_mapping[1L] <- section_column[1L]
  for (i in seq_along(interval_indices)) {
    current_interval <- interval_indices[i]

    if (current_interval != previous_interval) {
      section_idx <- section_idx + 1L
      section_mapping[section_idx] <- section_column[i]
      previous_interval <- current_interval

    }

  }

  n_pulses <- rep(0, n_sections)
  for (i in seq_along(section_mapping)) {
    n_pulses[i] <- pulses_per_section[section_mapping[i]]
  }

  offsets <- c(0, cumsum(n_pulses)[-n_sections])
  names(offsets) <- unique_indices

  offsets
}
