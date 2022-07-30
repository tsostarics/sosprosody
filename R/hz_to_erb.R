#' Convert Hz values to ERBs
#'
#' @param hz_val Numeric vector of Hz values
#'
#' @return Numeric vector of corresponding ERB values
#' @export
hz_to_erb <- function(hz_val) {
  # Equation from https://www2.ling.su.se/staff/hartmut/bark.htm
  vapply(hz_val,
         function(hz)
           11.17*log((hz + 312)/(hz + 14675))+43.0,
         1.0)
}

#' Convert ERB values to Hz values
#'
#' @param erb Numeric vector of ERB values
#'
#' @return Numeric vector of corresponding Hz values
#' @export
erb_to_hz <- function(erb) {
  # Back transformed from the equation in
  # https://www2.ling.su.se/staff/hartmut/bark.htm
  E <- exp((erb - 43)/11.17)
  (312 - 14675*E) / (E - 1)
}

