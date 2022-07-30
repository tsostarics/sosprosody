#' Increase Hz value by erbs
#'
#' Given a vector of Hz values, return a matrix obtained by adding to the values
#' in specified ERB increments.
#'
#' @param hz_vals Numeric vector of Hz values
#' @param erbs Numeric vector of ERB values to add to the Hz values
#'
#' @return Matrix where columns correspond to input `hz_vals` indices and rows
#' correspond to `erbs` indices
#' @export
#'
#' @examples
#'
#' add_erb(c(70, 110), 1:3)
add_erb <-  function(hz_vals, erbs) {
  sapply(hz_vals,
         function(hz) {
           vapply(erbs,
                  function(erb)
                    erb_to_hz(hz_to_erb(hz) + erb),
                  1.0)
         })
}
