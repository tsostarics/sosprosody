#' Convert textgrid object to list of dataframes
#'
#' Given a textgrid object, return a list of dataframes.
#' Each entry in the list is named according to the tier name.
#'
#' @param textgrid TextGrid object from `tg.read`
#'
#' @return List of dataframes for each tier
#' @export
textgrid_to_dataframes <- function(textgrid) {
  dfs <-
    lapply(textgrid,
           \(tier){
             tg_df <- data.frame(file = class(textgrid)[['name']],
                                 interval_start = tier[['t1']],
                                 interval_end = tier[['t2']],
                                 label = tier[['label']])
             attr(tg_df, "tiertype") <- tier[['type']]
             tg_df
           })
  dfs
}
