#' Difference between frequencies in ERBs
#'
#' @param f1 Frequency 1 in Hz
#' @param f2 Frequency 2 in Hz
#'
#' @return Difference between `f1` and `f2` on the ERB scale
#' @export
erb_difference <- function(f1, f2) {
  stopifnot(length(f1) == length(f2))

  vapply(seq_along(f1),
         function(i)
           hz_to_erb(f1[i]) - hz_to_erb(f2[i]),
         1.0
  )
}
