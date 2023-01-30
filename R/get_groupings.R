#' Get groupings from user and dataframe
#'
#' Helper function, extracts the groups from a grouped dataframe and the
#' user supplied groupings, then either merges them or returns the user's
#' preferences.
#'
#' @param grpd_df Dataframe, perhaps with groups
#' @param .grouping Character vector
#' @param .overridegroups Logical, defaults to `FALSE` to merge the existing
#' grouping structure and the user's specification. Set to `TRUE` to drop the
#' existing groups and group by the user specification.
#'
#' @return Character vector with no duplicates, either `.grouping` or the union
#' of `.grouping` and the groups of `grpd_df`.
.get_groupings <- function(grpd_df, .grouping, .overridegroups = FALSE) {
  if (.overridegroups)
    return(unique(.grouping))

  unique(c(as.character(dplyr::groups(grpd_df)), .grouping))
}
