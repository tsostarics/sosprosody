#' Piecewise interpolation of equally spaced pitch pulses
#'
#' This is an extension of `interpolate_equal_pulses` which extracts equal
#' pulses within particular sections of a time series. This was written as a
#' component of `average_pitchtracks`, where I needed to extract equally spaced
#' pulses within prenuclear and nuclear regions separately so I could then take
#' averages within the sections. This function can also be useful if you want a
#' lower resolution for one section but a higher resolution for another.
#'
#' It is highly recommended that you also have a column that denotes a numeric
#' index for each section. For example, if you have three sections that share
#' the label `"word"`, then these will all be treated as the same, ONE, section
#' if there are no other sections in between each word. So, a separate column
#' distinguishing word-1 from word-2 from word-3 is needed. This should be
#' passed to `index_by`. If you don't have duplicates, then the indices will be
#' taken from the (presumably already uniquely identifying) values in
#' `section_by`. If the results look weird, try adding a numeric index.
#'
#' Note that if you have two sections that share a boundary, you will get two
#' pulses at the same timepoint-- one associated with the end of the first
#' section, the other at the start of the second. If you plot the pulse indices
#' you'll encounter cases where there's no change from one pulse to another at
#' these shared timestamps. If you want to remove one of them, please refer to
#' `drop_overlapping_pulses()`.
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#'   and preferably with time normalization already applied
#' @param section_by Column name containing the section designations of the
#'   pitch contour
#' @param pulses_per_section A named integer vector of how many points to use
#'   for each section of the contour. There are three cases possible here.
#'
#'  - 1 and only 1 unnamed value is provided, which is recycled for all sections.
#'   For example: c(10) for 10 pulses for all sections.
#'  - All unique section names are specified with some number of pulses. So, if
#'   you have 5 sections, this vector should be named and of length 5. For
#'   example: c('a' = 10, 'b' = 30, 'c' = 20)
#'  - A partially named integer vector with 1 unnamed value. The names should be
#'   valid section names that exist in the `section_by` column. The 1 unnamed
#'   value will be recycled for all sections that are not specified. For
#'   example: c('a' = 10, 20) ==> c('a' = 10, 'b' = 20, 'c' = 20)
#'
#' @param index_by String or NULL, column name containing numeric indices of
#'   each interval to extract pulses from. This only needs to be provided if you
#'   have two adjacent intervals that have the same label in the column
#'   `section_by`. Without unique indices differentiating them, this function
#'   will treat them as one "long" interval. If you don't have this issue in
#'   your data, then this can be left as NULL.
#' @param x Quoted column name containing the timepoints, defaults to
#'   `"timepoint_norm"` (highly recommended to use `time_normalize` as a
#'   preprocessing step beforehand). Note that the results will have new
#'   time-normalized values with the same column name.
#' @param group Quoted column name indexing the unique recordings, defaults to
#'   `"file"`
#' @param y Quoted column name for which pitch values to use, defaults to `"hz"`
#' @param parallelize Deprecated, not used
#' @param sort_first Logical, defaults to TRUE, whether to sort the dataframe by
#'   the values in `x` for each group specified by `group`. If your dataframe is
#'   large, you should consider pre-sorting it before passing to this function &
#'   set this to FALSE. The sorting is needed to ensure that the pulse indices
#'   are in the correct order.
#' @param .grouping Deprecated, use `group`, @seealso [deprecate_argument()]
#' @param .sort Deprecated, use `sort_first`, @seealso [deprecate_argument()]
#' @param .pitchval Deprecated, use `y`, @seealso [deprecate_argument()]
#' @param time_by Deprecated, use `x`, @seealso [deprecate_argument()]
#' @param index_column Deprecated, use `index_by`, @seealso
#'   [deprecate_argument()]
#'
#' @return Dataframe of interpolated pitch pulses by section
#' @export
#'
#' @importFrom stats setNames
piecewise_interpolate_pulses <- function(pitchtier_df,
                                         x,
                                         y,
                                         pulses_per_section,
                                         section_by = NULL,
                                         index_by = NULL,
                                         group,
                                         sort_first,
                                         # Deprecated parameters
                                         parallelize = FALSE,
                                         .grouping,
                                         .sort,
                                         .pitchval,
                                         time_by,
                                         index_column) {

  deprecate_argument(x, time_by)
  deprecate_argument(y, .pitchval)
  deprecate_argument(group, .grouping)
  deprecate_argument(index_by, index_column)
  deprecate_argument(sort_first, .sort)


  requireNamespace('data.table',quietly = TRUE)
  pitchtier_df_cols <- colnames(pitchtier_df)

  if (is.null(section_by))
    section_by <- group

  stopifnot(section_by %in% pitchtier_df_cols)
  stopifnot(x %in% pitchtier_df_cols)
  full_groupings <- .get_groupings(pitchtier_df, group)


  if (dplyr::is.grouped_df(pitchtier_df))
    pitchtier_df <- dplyr::ungroup(pitchtier_df)


  # Order timepoints if needed (if not sorted the pulse indices will be wrong)
  if (sort_first)
    pitchtier_df <- dplyr::ungroup(dplyr::arrange(.group_by_vec(pitchtier_df, group),
                                                  .data[[x]],
                                                  .by_group = TRUE))

  # Guess unique interval indices if index_by is not provided
  if (is.null(index_by))
    index_by <- section_by

  # Use unique intervals and section names to set appropriate # of pulses
  unique_sections <- as.character(unique(pitchtier_df[[section_by]]))
  pulses_per_section <- .fill_pps(pulses_per_section, unique_sections)

  # If this should be run in parallel, use future_map
  other_grouping_table <- dplyr::reframe(pitchtier_df, .by = all_of(full_groupings))

  interpolated_df <-
    do.call(rbind,
      lapply(
        other_grouping_table[[group]],
        \(cur_file) {
          .interpolate_file(pitchtier_df[pitchtier_df[[group]] == cur_file,],
                            pulses_per_section = pulses_per_section,
                            index_by = index_by,
                            section_by = section_by,
                            x = x,
                            full_groupings = full_groupings,
                            y = y,
                            group = group)

        }))
  dplyr::left_join(interpolated_df, other_grouping_table, by = group, multiple = 'all')
}

.interpolate_file <- function(file_df,
                              pulses_per_section,
                              index_by,
                              section_by,
                              x,
                              full_groupings,
                              y,
                              group) {
  indices <- unique(file_df[[index_by]])

  # Interpolate each section and row-bind the results
  file_int_df_list <-
    lapply(indices,
           \(i) {
             .interpolate_section(file_df[file_df[[index_by]] == i,],
                                        pulses_per_section = pulses_per_section,
                                        index_by = index_by,
                                        section_by = section_by,
                                        x = x,
                                        full_groupings = full_groupings,
                                        y = y,
                                        group = group)
           })

  file_int_df <- do.call(rbind,  file_int_df_list)
  file_int_df[['pulse_i']] <- seq_len(nrow(file_int_df))

  file_int_df
}

.interpolate_section <- function(section_df,
                                 pulses_per_section,
                                 index_by,
                                 section_by,
                                 x,
                                 full_groupings,
                                 y,
                                 group) {
  if (nrow(section_df) < 2L)
    return(NULL)

  interval_idx <- section_df[[index_by]][1L]
  section_label <- section_df[[section_by]][1L]
  section_n_pulses <- pulses_per_section[as.character(section_label)]

  int_df <- interpolate_equal_pulses(section_df,
                                     n_pulses  = section_n_pulses,
                                     x   = x,
                                     y = y,
                                     group = group)

  int_df[[section_by]] <- section_label
    int_df[[index_by]] <- interval_idx

  int_df
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
