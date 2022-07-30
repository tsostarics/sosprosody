#' Convert pitchtier object to dataframe
#'
#' Convert pitchtier loaded by `rPraat::pt.read` to a dataframe.
#' If semitones are included via `add_semitones`, a `semitones_from` attribute
#' is added. Access with `attr(df, 'semitones_from')`.
#'
#' @param pitchtier Pitchtier object from `pt.read`
#' @param add_semitones Logical, default TRUE, whether to add semitone
#' differences from a baseline
#' @param add_erbs Logical, default TRUE, whether to add ERB units
#' @param ... Additional arguments passed to `hz_to_semitones`
#'
#' @return Dataframe representation of the given pitchtier object
#' @export
pitchtier_to_dataframe <- function(pitchtier,
                                   add_semitones = TRUE,
                                   add_erbs = TRUE,
                                   ...) {
  pt_df <- data.frame(file = gsub(".PitchTier$",
                                  "",
                                  attr(pitchtier, "class")[['name']]),
                      timepoint = pitchtier[['t']],
                      hz = pitchtier[['f']])
  if (add_semitones) {
    semitones <- hz_to_semitones(pt_df[['hz']], ...)
    pt_df[["semitone_difference"]] <- semitones
    attr(pt_df, "semitones_from") <- attr(semitones, 'semitones_from')
  }

  if (add_erbs) {
    pt_df[['erb']] <- hz_to_erb(pt_df[['hz']])
  }

  pt_df
}
