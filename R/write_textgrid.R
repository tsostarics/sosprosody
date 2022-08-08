#' Write textgrids to directory
#'
#' A  wrapper for `rPraat::tg.write` that will write a textgrid to the given
#' directory with the filename pulled from the class attributes.
#' Note that the filepath will be quietly sanitized with `fs::path_sanitize`
#' to ensure that the filename is valid.
#'
#' @param textgrid TextGrid object
#' @param directory Directory to write textgrid file to
#' @param filename New filename for the textgrid file. If `NULL`, the default,
#' the file name will be extracted from the textgrid object.
#' @param format See `rPraat::tg.write`, defaults to "short"
#'
#' @return Invisibly returns the new file path
#' @export
write_textgrid <- function(textgrid, directory, filename = NULL, format = "short") {
  if (!is.null(filename))
    filename <- class(textgrid)['name']

  filepath <- file.path(directory, fs::path_sanitize(filename))
  rPraat::tg.write(textgrid, filepath, format)

  invisible(filepath)
}
