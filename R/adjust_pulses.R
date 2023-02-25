#' Adjust pulse indices due to junctures
#'
#' When using `piecewise_interpolate_pulses`, you will get 2 pulses when two
#' sections share a boundary (ie ending of section 1 = start of section 2) as
#' shown in the example:
#'

#'
#' This isn't an issue if you're plotting/modeling with the timestamp itself,
#' as these will overlap as needed. However, if you try to plot/model with
#' the pulse index, you'll have a part of your time series where there is
#' no change between the adjacent pulses.
#'
#' This function takes the output of `piecewise_interpolate_pulses`, identifies
#' pulses that share a timestamp, and either keep the first one (`left`) or
#' the second one (`right`) as specified by `keep`. If you want to retain
#' both pulses, you can set keep to `both`, but note that this will return
#' the input dataframe unchanged.
#'
#' Lastly, if any pulses are removed, the indices are shifted accordingly.
#' Note that this means if you had two sections with 30 pulses each, this
#' will cause one to be 30 pulses and one to be 29. If you want both to be
#' 30, you should run `piecewise_interpolate_pulses` but extract 30 and 31
#' pulses.
#'
#' @param pulse_df Dataframe containing integer pulse indices and numeric
#' timestamps
#' @param keep String, either `'left'`, `'right'`, or `'both'`
#' @param time_by String, which column contains the numeric timestamps
#' @param pulse_col String, which column contains the integer timestamps
#' @param .grouping String, which column should be used to identify unique
#' groups (eg `file` for filenames)
#'
#' @return `pulse_df`, but with pulses removed and indices shifted accordingly
#' @export
#'
#' @examples
#' \dontrun{
#' Original dataframe (also output if keep = both)
#' section | timestamp | pulse_i
#' a       | 0.0       | 1
#' a       | 1.0       | 2
#' a       | 2.0       | 3 <--
#' b       | 2.0       | 4 <--
#' b       | 3.0       | 5
#' b       | 4.0       | 6 <--
#' c       | 4.0       | 7 <--
#' c       | 5.0       | 8
#' c       | 6.0       | 9
#'
#' if keep = left:
#' section | timestamp | pulse_i
#' a       | 0.0       | 1
#' a       | 1.0       | 2
#' a       | 2.0       | 3 <--
#' b       | 3.0       | 4
#' b       | 4.0       | 5 <--
#' c       | 5.0       | 6
#' c       | 6.0       | 7
#'
#' if keep = right:
#' section | timestamp | pulse_i
#' a       | 0.0       | 1
#' a       | 1.0       | 2
#' b       | 2.0       | 3 <--
#' b       | 3.0       | 4
#' c       | 4.0       | 5 <--
#' c       | 5.0       | 6
#' c       | 6.0       | 7
#'
#' #########
#'
# tstdf <- data.frame(tstfile = "f",
#                     tstsec = c(rep("later",6),rep("earlier",5)),
#                     tsthz = c(seq(10,50,length.out = 6),
#                               c(10,20,30,20,10)),
#                     tsttp = c(5:10,
#                               1:5))
#' int_df <-
#'   piecewise_interpolate_pulses(tstdf,
#'                                section_by = "tstsec",
#'                                pulses_per_section = c("earlier" = 20,
#'                                                       "later" = 30),
#'                                time_by = "tsttp",
#'                                .pitchval = 'tsthz',
#'                                .grouping = 'tstfile')
#'
#' adjusted_df_l <- adjust_pulses(int_df,'l','tsttp','pulse_i',.grouping = 'tstfile')
#' adjusted_df_r <- adjust_pulses(int_df,'r','tsttp','pulse_i',.grouping = 'tstfile')
#' adjusted_df_b <- adjust_pulses(int_df,'b','tsttp','pulse_i',.grouping = 'tstfile')
#' }
#'
adjust_pulses <- function(pulse_df,
                          keep,
                          time_by = 'timepoint',
                          pulse_col = 'pulse_i',
                          .grouping = 'file') {
  keep <- match.arg(keep, c('left','right','both'))

  # if 'keep' is 'both', return input data frame, as we want to retain both
  # pulses at each juncture
  if (keep == 'both')
    return(pulse_df)

  adjusted_df <-
    # Split pulse data frame into groups
    pulse_df |>
    .group_by_vec(.grouping) |>
    dplyr::group_split() |>
    # For each group, remove pulses at each juncture within the group
    purrr::map_dfr(
      function(grp_df) {
        pulses <- grp_df[[pulse_col]]
        time_diffs <- c(NA, diff(grp_df[[time_by]]))
        juncture_indices <- which(time_diffs == 0)
        n_pulses <- length(time_diffs)


        # section | time_diff | pulse_i
        #  a      |    NA     | 1     // shift = 0
        #  a      |    1      | 2
        #  a      |    4      | 3     <-- If left, keep this
        #                              ^- If right, remove this
        #                             // new juncture => shift = 1
        #  b      |    0      | 4     <-- If right, keep this (pulse_i-shift)
        #                              ^- If left, remove this
        #  b      |    1      | 5     <-- Whether right or left, pulse_i-shift
        #  b      |    1      | 6     <-- if left, keep this, if right remove
        #                             // new juncture => shift = 2
        #  c      |    0      | 7     <-- if left, remove, if right keep + shift
        #  c      |    0      | 8     <-- Needs to be shifted by 2 due to pulse
        #                                 deletions at previous 2 junctures
        if (keep == 'right')
          juncture_indices <- juncture_indices - 1


        shift <- 0
        row_i <- 1
        for (j in c(juncture_indices, n_pulses)) {

          while (row_i <= j) {
            pulses[row_i] <- pulses[row_i] - shift
            row_i <- row_i + 1
          }

          shift <- shift + 1

        }
        grp_df <- grp_df[-juncture_indices,]
        grp_df[[pulse_col]] <- pulses[-juncture_indices]
        grp_df
      })


  adjusted_df
}
