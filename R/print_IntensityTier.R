#' Print IntensityTier
#'
#' See `print.PitchTier`, this does the same thing but with intensity tiers
#' read in by `rPraat::it.read()`
#'
#' @param x IntensityTier object
#' @param ... Not used
#'
#' @return invisibly return `x`
#' @export
print.IntensityTier <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' Format console intensitytier plot
#'
#' See `format.PitchTier`
#'
#' @param x IntensityTier from `it.read`
#' @param ... Unused
#' @param .horiz_res Horizontal resolution
#' @param .vert_res Vertical resolution
#'
#' @return Returns formatted string to pass to `print.IntensityTier`
#' @export
format.IntensityTier <- function(x, ..., .horiz_res = 80L, .vert_res = 10L) {
  .formatTier(x, ..., tier_var = 'i', .horiz_res, .vert_res)
}
