interpolate_equal_pulses <- function(pitchtier_df,
                                     n_pulses = 30,
                                     time_by = "timepoint_norm",
                                     .pitchval = "hz",
                                     .grouping = 'file') {
  n_pulses <- as.integer(n_pulses)

  interpolated_df <-
    pitchtier_df |>
    summarize(min_time = min(.data[[.time_by]]),
              max_time = max(.data[[.time_by]]),
              .groups = "keep") |>
    summarize(!!sym(.time_by) := seq(min_time,
                                     max_time,
                                     length.out = n_pulses),
              .groups = "keep")

  # Interpolate n_pulses between first and last timepoints
  group_vals <- unique(pitchtier_df[[.grouping]])

  map_dfr(group_vals,
          \(grp) {
            int_df <- interpolated_df[interpolated_df[[.grouping]] == grp,]
            pt_df <- pitchtier_df[pitchtier_df[[.grouping]] == grp,]

            int_df[[.pitchval]] <- interpolate_pitchpoints(int_df[[.time_by]],
                                                           pt_df[[.time_by]],
                                                           pt_df[[.pitchval]])
            int_df
          })
}
