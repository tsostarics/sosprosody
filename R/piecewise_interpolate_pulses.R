#' Piecewise interpolation of equally spaced pitch pulses
#'
#' This is an extension of `interpolate_equal_pulses` which extracts equal pulses
#' within particular sections of a time series. This was written as a component of
#' `average_pitchtracks`, where I needed to extract equally spaced pulses
#' within prenuclear and nuclear regions separately so I could then take
#' averages within the sections. This function can also be useful if you
#' want a lower resolution for one section but a higher resolution for another.
#'
#' It is highly recommended that you also have a column that denotes a numeric
#' index for each section. For example, if you have three sections that share
#' the label `"word"`, then these will all be treated as the same, ONE, section
#' if there are no other sections in between each word. So, a separate column
#' distinguishing word-1 from word-2 from word-3 is needed. This should be
#' passed to `index_column`. If you don't have this precomputed, this function
#' will try to guess by assigning indices to contiguous groups, but, if you
#' have any sections that overlap, you may get incorrect results.
#'
#' Note that if you have two sections that share a boundary, you will get
#' two pulses at the same timepoint-- one associated with the end of the first
#' section, the other at the start of the second. If you plot the pulse indices
#' you'll encounter cases where there's no change from one pulse to another
#' at these shared timestamps. If you want to remove one of them, please refer
#' to `drop_overlapping_pulses()`.
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' and preferably with time normalization already applied
#' @param section_by Column name containing the section designations of the pitch contour
#' @param pulses_per_section A named integer vector of how many points to use
#' for each section of the contour. There are three cases possible here.
#'
#'  - 1 and only 1 unnamed value is provided, which is recycled for all sections.
#'  For example: c(10) for 10 pulses for all sections.
#'  - All unique section names are specified with some number of pulses. So, if
#'  you have 5 sections, this vector should be named and of length 5. For
#'  example: c('a' = 10, 'b' = 30, 'c' = 20)
#'  - A partially named integer vector with 1 unnamed value. The names should be
#'  valid section names that exist in the `section_by` column. The 1 unnamed
#'  value will be recycled for all sections that are not specified. For example:
#'  c('a' = 10, 20) ==> c('a' = 10, 'b' = 20, 'c' = 20)
#'
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

  # Order timepoints (if not sorted the pulse indices will be wrong)
  if (.sort)
    pitchtier_df <-
    pitchtier_df |>
    dplyr::arrange(.data[[section_by]], .by_group = TRUE) |>
    dplyr::arrange(.data[[time_by]], .by_group = TRUE)


  # Get unique indices for each interval, guess if not provided
  if (is.null(index_column)) {
    as.integer(factor())
    pitchtier_df <- pitchtier_df |>
      dplyr::mutate(sosprosody_interval_i  = guess_interval_indices(.data[[section_by]]))
    index_column <- 'sosprosody_interval_i'
    DROP_INDEX <- TRUE
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

      interpolated_df <-
        file_df |>
        .group_by_vec(index_column) |>
        dplyr::group_split() |>
        map_method( \(section_df) {
          section_df <- .group_by_vec(section_df, full_groupings) # Needed to retain grouping columns, average_pitchtracks breaks otherwise
          interval_idx <- section_df[[index_column]][1L]
          section_label <- section_df[[section_by]][1L]
          section_n_pulses <- pulses_per_section[as.character(section_label)]

          int_df <- interpolate_equal_pulses(section_df,
                                             n_pulses  = section_n_pulses,
                                             time_by   = time_by,
                                             .pitchval = .pitchval,
                                             .grouping = .grouping)

          int_df[[section_by]] <- section_label

          if (!DROP_INDEX)
            int_df[[index_column]] <- interval_idx

          int_df
        })

      interpolated_df[['pulse_i']] <- seq_len(nrow(interpolated_df))
      interpolated_df
    })
}

.fill_pps <- function(pulses_per_section, unique_sections) {
  # Temp value to recycle later
  recycled_pulse_value <- 10

  # Get the names from pulses_per_section and get the number of unnamed elements
  pps_names <- names(pulses_per_section)
  is_unnamed_value <- pps_names == ""
  if (identical(is_unnamed_value, logical(0)))
    is_unnamed_value <- TRUE
  n_empty <- sum(is_unnamed_value)

  is_section_name <- pps_names[!is_unnamed_value] %in% unique_sections

  # If no recyclable value is set
  if (n_empty == 0) {

    # If there are some names set
    if (!is.null(pps_names)) {
      # # of sections in pulses_per_sections must equal # of unique sections
      if (!is.null(pps_names) & length(pps_names)  != length(unique_sections)) {
        sections_not_set <- paste0(unique_sections[!unique_sections %in% pps_names], collapse = ", ")
        stop(glue::glue("Section not specified in pulses_per_section but no recyclable value is set: {sections_not_set}"))
      }

      # Names of pulses_per_sections must all exist in unique sections
      if (!all(is_section_name)) {
        not_existing_sections <- paste0(pps_names[!is_section_name], collapse = ', ')
        stop(glue::glue("Section names not found: {not_existing_sections}"))
      }

      return(pulses_per_section)
    }

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
