#' Valid list-representation of textgrid
#'
#' Checks if a given list can correspond to a valid textgrid.
#' Useful if you're using R to create new textgrid tiers to add to an existing
#' textgrid.
#' Note that this is not `is_textgrid` because it does not check for the
#' relevant class attributes associated with textgrids loaded with `rPraat::tg.read`
#'
#' @param tier_list Named list with relevant tier information
#'
#' @return TRUE if the list can be used as a textgrid sans class attributes
#' @export
is_tier_list <- function(tier_list) {
  # Must be a named list
  if (is.null(names(tier_list)))
    stop("Tier list must be named")

  # Names must correspond to name specified in tier
  tier_names <- vapply(tier_list, \(tier) tier[['name']], "char")
  non_matching_names <- names(tier_list) != tier_names
  if (any(non_matching_names))
    stop("Outer names of tier list must correspond to `name` entry of each tier")

  # Tier types must be either interval or point
  tier_types <- vapply(tier_list, \(tier) tier[['type']], "char")
  invalid_types <- !tier_types %in% c('interval', 'point')
  if (any(invalid_types))
    stop(glue::glue("{tier_names[invalid_types]} have invalid tier types {tier_types[invalid_types]}"))

  # The two tiers specify time differently
  correct_time_specification <-
    vapply(tier_list,
           \(tier)
           ifelse(
             tier[['type']] == "interval",
             all(c('t1','t2') %in% names(tier)),
             "t" %in% names(tier)
           ),
           TRUE)
  if (!all(correct_time_specification))
    stop("Interval tiers must specify t1 and t2, point tiers must specify t")

  # Interval tiers must have equal length of t1 and t2
  matching_time_lengths <- vapply(tier_list[tier_types == "interval"],
                                  \(tier)
                                  length(tier[['t1']]) == length(tier[['t2']]),
                                  TRUE)

  if (!all(matching_time_lengths))
    stop(glue::glue("t1 and t2 do not have matching lengths for tiers {tier_names[!matching_time_lengths]}"))

  # The end of an interval (t2) should equal the start of the next interval (t1)
  # excluding the first and last points (file boundaries)
  timepoints_aligned <- vapply(tier_list[tier_types == 'interval'],
                               \(tier)
                               all(tier[['t1']][-1] == tier[['t2']][-length(tier[['t2']])]),
                               TRUE)
  if (!all(timepoints_aligned))
    stop(glue::glue("Interval timepoints not aligned on tiers {tier_names[!timepoints_aligned]}, `t2[1:n-1]` should equal `t1[2:n]`"))

  # The ending times of all interval tiers must equal each other
  max_times <- vapply(tier_list[tier_types == "interval"],
                      \(tier)
                      max(as.numeric(tier[['t2']])),
                      1.0)

  if (!all((max_times - max_times) == 0))
    stop("Last values of t2 must be the same across all tiers")

  # Time points must be monotonically increasing
  is_ascending <- vapply(tier_list,
                         \(tier)
                         ifelse(
                           tier[['type']] == "interval",
                           all(sign(diff(tier[['t1']])) >= 0) &&
                             all(sign(diff(tier[['t2']])) >= 0),
                           all(sign(diff(tier[['t']])) >= 0)
                         ),
                         TRUE)

  if (!all(is_ascending))
    stop(glue::glue("Tiers {tier_names[!is_ascending]} do not have ascending time values"))

  # The number of labels must equal the number of intervals/points
  equal_n_labels <- vapply(tier_list,
                           \(tier)
                           ifelse(
                             tier[['type']] == "interval",
                             length(tier[['label']]) == length(tier[['t1']]),
                             "t" %in% names(tier)
                           ),
                           TRUE)
  if (!all(equal_n_labels))
    stop(glue::glue('Number of labels for tiers {tier_names[!equal_n_labels]} do not equal number of points/labels.\nBlank labels should be ""'))

  TRUE
}
