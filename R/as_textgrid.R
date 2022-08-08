#' Convert list to textgrid object
#'
#' Similar to `rPraat::as.tg` but with more validation on the input list.
#' The output of this can be passed to `add_tier` with another textgrid.
#'
#' Note that the data types of each part of the tier will be coerced to the
#' correct value automatically via `coerce_tier_types`. Unexpected results may
#' occur if you set up a tier list with incorrect or missing values.
#'
#' @param tier_list Named list with relevant tier information, see `is_tier_list`
#' @param filename String, filename to use, defaults to `""`
#' @param tmin Numeric, time min for the textgrid, defaults to `0`
#' @param tmax Numeric, time max for the textgrid, if `NULL` (the default),
#' this value will be extracted from an interval tier.
#' @param .strict_t1 Logical, default `TRUE` whether to ensure that every
#' interval tier starts at `tmin`. May give unexpected results or corrupted
#' textgrids if set to `FALSE`. Will also check that the new `tmin` is <= the
#' first time point in each tier (t1 for intervals).
#' @param .strict_t2 Logical, default `TRUE` whether to ensure that every
#' interval tier ends at `tmax`. May give unexpected results or corrupted
#' textgrids if set to `FALSE`. Will also check that the new `tmax` is >= the
#' last time point in each tier (t2 for intervals).
#' @param .quiet Whether to warn user if  `t1` or `t2` values are changed as a result of
#' `.strict_t1` and `.strict_t2`
#'
#' @return TextGrid object with the given filename
#' @export
as_textgrid <- function(tier_list,
                        filename = "",
                        tmin = 0,
                        tmax = NULL,
                        .strict_t1 = TRUE,
                        .strict_t2 = TRUE,
                        .quiet = FALSE) {
  # Coerce types to correct formats just in case
  tier_list <- coerce_tier_types(tier_list)

  # Set outer names in case they aren't set
  tier_names <- vapply(tier_list, \(tier) tier[['name']], "char")
  names(tier_list) <- tier_names

  # Verify that we can treat this as a textgrid, will error if we can't
  is_tier_list(tier_list)
  is_interval <- vapply(tier_list, \(tier) tier[['type']] == "interval", TRUE)

  # If tmax is not specified, then there must be an interval tier to pull this
  # value from. Point tiers don't include it in their timestamps.
  if (is.null(tmax)){
    if (!any(is_interval))
      stop("If tmax is not specified, there must be an interval tier")

    tmax <- max(tier_list[[which(is_interval)[1]]][['t2']])
  }

  # Verify that tmax corresponds to the last t2 value for each interval
  # Override the value there if it's not, but warn the user that this happens
  if (.strict_t2) {
    valid_tmax <-
      vapply(tier_list,
             \(tier) ifelse(tier[['type']] == "interval",
                            tier[['t2']] <= tmax,
                            tier[['t']] <= tmax),
             TRUE)

    if (!all(valid_tmax))
      stop("New tmax must be >= last time point in each tier")

    for (tier_name in names(tier_list)[is_interval]) {
      last_t2 <- length(tier_list[[tier_name]][['t2']])
      if (tier_list[[tier_name]][['t2']][last_t2] != tmax){
        tier_list[[tier_name]][['t2']][last_t2] <- tmax
        if (!.quiet)
          warning(glue::glue("Final t2 in tier {tier_name} changed to tmax {tmax}"))
      }
    }
  }

  if (.strict_t1) {
    valid_tmin <-
      vapply(tier_list,
             \(tier) ifelse(tier[['type']] == "interval",
                            tmin <= tier[['t1']],
                            tmin <= tier[['t']]),
             TRUE)

    if (!all(valid_tmin))
      stop("New tmin must be <= first time point in each tier")

    for (tier_name in names(tier_list)[is_interval]) {
      first_t1 <- tier_list[[tier_name]][['t1']][1L]
      if (first_t1 != tmin){
        tier_list[[tier_name]][['t1']][1L] <- tmin
        if (!.quiet)
          warning(glue::glue("First t1 in tier {tier_name} changed to tmin {tmin}"))
      }
    }
  }

  class_value <-
    c("list",
      tmin = as.character(tmin),
      tmax = as.character(tmax),
      type = "TextGrid",
      name = fs::path_sanitize(filename))

  class(tier_list) <- class_value
  tier_list
}

#' Coerce tier data types to correct types
#'
#' Helper for `as_textgrid`, will ensure that the data types are correct
#' for textgrids. You should use this on a tier list before passing to
#' `is_tier_list`.
#'
#' @param tier_list List representation of a textgrid tier, see `is_tier_list`
#'
#' @return The input tier list but coerced to the correct datatypes
#' @export
coerce_tier_types <- function(tier_list) {
  tier_list <- lapply(tier_list,
                      \(tier) {
                        tier[['name']] <- as.character(tier[['name']])
                        tier[['type']] <- as.character(tier[['type']])
                        tier[['label']] <- as.character(tier[['label']])
                        if (tier[['type']] == 'interval') {
                          tier[['t1']] <- as.numeric(tier[['t1']])
                          tier[['t2']] <- as.numeric(tier[['t2']])
                        } else {
                          tier[['t']] <- as.numeric(tier[['t']])
                        }
                        tier
                      })
  tier_list
}
