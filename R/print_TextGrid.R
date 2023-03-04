#' Format console TextGrid plot
#'
#' Formats a textgrid object to print a tiered representation to the console.
#' Filename is shown at the top, with tiers printed in descending order as they
#' would be displayed in Praat.
#' Interval tiers are shown with `[square brackets]` while point tiers are shown
#' with `|bars on the ends|`
#'
#' Summary information about the number of labeled annotations out of the total
#' number of annotations on the tier is provided, along with the total duration.
#'
#' @param x TextGrid object
#' @param .horiz_res Horizontal resolution, integer number of characters
#' wide to print. Default is 80, recommended to be at least 50. Coerced to be 2
#' if given a value less than 2, although 2 isn't particularly useful
#' @param .vert_res Vertical resolution, integer number of characters high to
#' print. Will be coerced to an even number if given an odd number & coerced
#' to 4 if given too small a value
#' @param ... Not used
#'
#' @return Returns formatted string to pass to `print.TextGrid`
#' @export
format.TextGrid <- function(x, ..., .horiz_res = 80L, .vert_res = 5L) {
  if (.horiz_res < 2)
    .horiz_res <- 2
  if (.vert_res < 1)
    .vert_res <- 1

  filename <- class(x)[["name"]]

  # Limit number of tiers to print as needed
  n_to_print <- min(.vert_res, length(x))
  n_chopped <- length(x) - n_to_print
  chopped_notice <- ""
  if (n_chopped > 0)
    chopped_notice <- crayon::italic(glue::glue("\n...Not showing {n_chopped} other tiers", .trim = FALSE))
  x <- x[seq_len(n_to_print)]

  # Create strings to put inside intervals
  .horiz_res <- .horiz_res - 2

  tier_strings <-
    vapply(x,
           \(tier) {
             n_annotations <- length(tier[['label']])
             n_labels <- sum(tier[['label']] != '')

             # Point tiers keep time in t, intervals in t1 & t2
             which_from_t <- "t1"
             which_to_t <- "t2"
             if (tier[['type']] == "point") {
               which_from_t <- "t"
               which_to_t <- "t"
             }
             from_t <- round(min(tier[[which_from_t]]), 2)
             to_t <- round(max(tier[[which_to_t]]), 2)
             string <- glue::glue("{tier[['name']]}: {n_labels}/{n_annotations} labeled {tier[['type']]}s from {from_t} to {to_t}")
             strtrim(string, .horiz_res)
           },
           "char")

  # Left-align all the strings within the tiers
  tier_lengths <- nchar(tier_strings)
  longest_tier_len <- max(tier_lengths)
  leading_pad_len <- round((.horiz_res - longest_tier_len ) / 2 -.5, 0)
  leading_pad <- strrep(" ", leading_pad_len) # same for all tiers

  # Calculate how many spaces to pad on the right, then format the strings
  # to display tiers with [] for intervals and || for points
  formatted_strings <-
    vapply(seq_along(x),
           \(tier_i) {
             tier_type <- x[[tier_i]][['type']]
             trailing_pad_len <- .horiz_res - leading_pad_len - tier_lengths[[tier_i]]
             trailing_pad <- strrep(" ", trailing_pad_len)
             brackets <- c("[", "]")
             if (tier_type == "point")
               brackets <- c("|", "|")
             paste0(brackets[1],
                    leading_pad,
                    tier_strings[[tier_i]],
                    trailing_pad,
                    brackets[2])
           }, "char")

  # Format to a single string and return
  paste0(paste0(c(filename, formatted_strings), collapse = "\n"),chopped_notice)
}

#' Print TextGrid object
#'
#' See `format.TextGrid`
#'
#' @param x TextGrid from `tg.read`
#' @param ... Unused
#'
#' @return invisibly returns `x`
#' @export
print.TextGrid <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}
