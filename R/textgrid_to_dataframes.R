#' Convert textgrid object to list of dataframes
#'
#' Given a textgrid object, return a list of dataframes.
#' Each entry in the list is named according to the tier name.
#'
#' @param textgrid TextGrid object from `tg.read`
#' @param .remove_filename Logical, whether to remove the `.TextGrid` part from
#' the file name
#'
#' @return List of dataframes for each tier
#' @export
textgrid_to_dataframes <- function(textgrid, .remove_filename = TRUE) {
  dfs <-
    lapply(textgrid,
           \(tier){
             filename <-  class(textgrid)[['name']]

             if (.remove_filename)
               filename <- gsub("\\.TextGrid$", "", filename, perl=TRUE)

             make_tier_df(tier, filename)
           })
  dfs
}

make_interval_tier_df <- function(tier, filename) {
  data.frame(file = filename,
             interval_start = tier[['t1']],
             interval_end = tier[['t2']],
             label = tier[['label']],
             interval_i = seq_along(tier[['t1']]))
}


make_point_tier_df <- function(tier, filename) {
  data.frame(file = filename,
             point_time = tier[['t']],
             label = tier[['label']],
             point_i = seq_along(tier[['label']]))
}

make_tier_df <- function(tier, filename) {
  if (tier[['type']] == "interval") {
    tier_df <- make_interval_tier_df(tier, filename)
  } else {
    tier_df <- make_point_tier_df(tier, filename)
  }

  attr(tier_df, "tiertype") <- tier[['type']]

  tier_df
}
