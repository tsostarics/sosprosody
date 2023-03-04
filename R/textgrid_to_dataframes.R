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
             tg_df <- data.frame(file = class(textgrid)[['name']],
                                 interval_start = tier[['t1']],
                                 interval_end = tier[['t2']],
                                 label = tier[['label']])

             if (.remove_filename)
               tg_df[['file']] <- gsub("\\.TextGrid$", "", tg_df[['file']],
                                       perl = TRUE)

             tg_df[['interval_i']] <- seq_len(nrow(tg_df))
             attr(tg_df, "tiertype") <- tier[['type']]
             tg_df
           })
  dfs
}
