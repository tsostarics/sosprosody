#' Title
#'
#' @param section_column Numeric, Integer, or Character vector of values
#'
#' @return Integer vector of indices for each contiguous group of values
guess_interval_indices <- function(section_column) {
  # stopifnot(typeof(section_column) %in% c('character','numeric','integer'))
  UseMethod('guess_interval_indices')
}

#' @export
guess_interval_indices.character <- function(section_column) {
  guess_interval_indices_string(section_column)
}

#' @export
guess_interval_indices.integer <- function(section_column) {
  guess_interval_indices_integer(section_column)
}

#' @export
guess_interval_indices.numeric <- function(section_column) {
  guess_interval_indices_double(section_column)
}

#' @export
guess_interval_indices.logical <- function(section_column) {
  guess_interval_indices_integer(section_column)
}

#' @export
guess_interval_indices.default <- function(section_column) {
  stop("Class of section column must be coerceable to character, integer, numeric, or double")
}
