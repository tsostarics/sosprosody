#' Format console pitchtier plot
#'
#' Formats a pitchtier object's pitch track via ASCII art.
#' Header: Filename of the object and total number of pitch pulses
#' Y-Axis: Maximum and Minimum F0 values, rounded to nearest integer
#' X-Axis: File start time (usually 0) and end time
#'
#' This looks like a lot but the execution time on average is only about
#' a millisecond.
#'
#' @param x PitchTier object
#' @param .horiz_res Horizontal resolution, integer number of characters
#' wide to print. Will be coerced to be 10 if given too small a value.
#' @param .vert_res Vertical resolution, integer number of characters high to
#' print. Will be coerced to an even number if given an odd number & coerced
#' to 4 if given too small a value
#' @param ... Not used
#'
#' @return Returns formatted string to pass to `print.PitchTier`
#' @export
format.PitchTier <- function(x, ..., .horiz_res = 80L, .vert_res = 10L) {
  .formatTier(x, ..., tier_var = 'f', .horiz_res, .vert_res)
}

#' Print pitchtier object
#'
#' See `format.PitchTier`
#'
#' @param x Pitchtier from `pt.read`
#' @param ... Unused
#'
#' @return invisibly returns `x`
#' @export
print.PitchTier <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

