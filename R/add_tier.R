#' Add tier to existing textgrid
#'
#' Given a textgrid object (like one returned by `rPraat::tg.read`) add new tiers
#' speciied by `new_tier_list`. The new tiers will be added to the bottom of the
#' textgrid. This function is useful if you calculate new interval or point
#' tiers programmatically with R and want to add them back to a textgrid.
#' See `rPraat::tg.write` for an existing implementation to write the new
#' textgrid and `sosprosody::write_textgrid` for a wrapper that fills the
#' filename parameter from the textgrid object directly.
#'
#' @param textgrid Textgrid object to add to
#' @param new_tier_list New tiers to add, durations must be contained within
#' the tmin and tmax of the textgrid
#' @param filename New filename for the textgrid. If `NULL`, the default,
#' the filename will be the same as that of the input `textgrid`
#' @param ... Additional logical arguments passed to `as_textgrid`
#'
#' @return Textgrid object with new tiers added at the bottom.
#' @export
add_tier <- function(textgrid,
                     new_tier_list,
                     filename = NULL,
                     ...) {
  if (is.null(filename)) {
    filename <- class(textgrid)['name']
  }

  new_tier_list <- as_textgrid(new_tier_list,
                               filename = "",
                               tmin = as.numeric(class(textgrid)['tmin']),
                               tmax = as.numeric(class(textgrid)['tmax']),
                               ...)
  n_existing_tiers <- length(textgrid)

  for (tier_i in seq_along(new_tier_list)) {
    textgrid[n_existing_tiers + tier_i] <- new_tier_list[tier_i]
  }


  class(textgrid)['name'] <- as.character(filename)
  textgrid
}
