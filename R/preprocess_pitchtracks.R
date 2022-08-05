#' Common pitch track preprocessing steps
#'
#' Wrapper function around other functions to apply various preprocessing
#' operations to a pitchtier df containing multiple files.
#' If you don't want to apply a particular operation, leave it blank.
#' Note that the order of the arguments as presented in this description is
#' the order of preprocesing operations. If you do them separately, I recommend
#' using the order shown here.
#'
#' @param pitchtier_df Pitchtier dataframe, output of `batch_process_pitchtiers`
#' @param nuclear_df Pass the output of `get_nuclear_tg` to code whether a pitch
#' pulse lies in the nuclear region or not via `code_nuclear_region`
#' @param runmed_k `k` for `runmed`, to call `running_median_smooth`. Changes
#' the Hz values.
#' @param time_normalize Logical, whether to normalize the pitch track to be
#' between (0, 1] via `time_normalize`. Changes the time
#' @param .from_smooth For `running_median_smooth`, quoted variable name for
#' column containing Hz values. Defaults to `hz`
#' @param .to_smooth For `running_median_smooth`, quoted variable name for column to
#' hold new Hz values. Defaults to `hz_norm`.
#' @param .from_time For `time_normalize`, quoted variable name for column
#' containing timepoint values. Defaults to `timepoint`.
#' @param .to_time For `time_normalize`, quoted variable name for column to
#' hold new timepoint values. Defaults to `timepoint_norm`.
#' @param .fromzero For `time_normalize`, whether results should start from 0
#'
#' @return Dataframe with modified columns
#' @export
preprocess_pitchtracks <- function(pitchtier_df,
                                   nuclear_df,
                                   runmed_k,
                                   time_normalize,
                                   .from_smooth = 'hz',
                                   .to_smooth = 'hz_runmed',
                                   .from_time = 'timepoint',
                                   .to_time = 'timepoint_norm',
                                   .fromzero = TRUE) {

  if (!missing(nuclear_df)) {
    stopifnot(is.data.frame(nuclear_df))
    pitchtier_df <- code_nuclear_pulses(pitchtier_df, nuclear_df)
  }

  if (!missing(runmed_k) && is.numeric(runmed_k))
    pitchtier_df <- running_median_smooth(pitchtier_df, runmed_k, .from_smooth, .to_smooth)

  if (!missing(time_normalize) && time_normalize)
    pitchtier_df <-  time_normalize(pitchtier_df, .from_time, .to_time)

  pitchtier_df
}
