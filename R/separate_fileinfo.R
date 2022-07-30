#' Separate fileinfo
#'
#' Wrapper around tidyr::separate
#'
#' @param dat Textgrid or pitchtier dataframe
#' @param .col See `?tidyr::separate`
#' @param .sep See `?tidyr::separate`
#' @param .into See `?tidyr::separate`
#'
#' @return `dat` with columns specified by .into
#' @export
separate_fileinfo <- function(dat,
                              .col = "file",
                              .sep = "_",
                              .into = c("utterance", "tune", "take")) {
    tidyr::separate(dat,
                    col = .col,
                    sep = .sep,
                    into = .into,
                    remove = FALSE)

}
