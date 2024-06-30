#' Deprecate argument
#'
#' Some functions had a clunky API with inconsistent nomenclature. This is a
#' helper function to deprecate some of the parameters in a consistent way
#' while allowing old scripts to work.
#'
#' @details
#' This function is designed to be used in `piecewise_interpolate_pulses` and
#' `average_pitchtracks`. Here is a list of the relevant deprecated parameters:
#'
#'  - `time_by` -> `x`
#'  - `.pitchval` -> `y`
#'  - `index_column` -> `index_by`
#'  - `.grouping` -> `group`
#'  - `.sort` -> `sort_first`
#'
#' This function will assign the new argument the value of the old argument
#' iff the new argument is missing and the old argument is provided a value.
#' A warning for each parameter is shown once per session.
#' This function will only execute if the parent environment is not the global
#' environmet; in other words, this function should not be called directly.
#' This is to avoid making assignments in the global environment or overwriting
#' data in the global environment.
#'
#' @param new_arg New argument, such as `x`
#' @param old_arg Old argument, such as `time_by`
#' @param .frequency String to pass to rlang::warn, set to `"always"` for
#' debugging and running tests, otherwise should always be `"once"` (default)
#'
#' @return Nothing, makes an assignment in the parent environment
#' @export
#'
#' @examples
#' \dontrun{
#' foo <- function(x, y) {
#' deprecate_argument(x, y)
#'
#' print(x)
#' }
#'
#' foo(y=4) # Prints 4 and throws a warning
#' }
deprecate_argument <- function(new_arg, old_arg, .frequency = "once") {
  if (!identical(parent.frame(), globalenv())) {
    new_arg_name <- as.character(rlang::ensym(new_arg))
    old_arg_name <- as.character(rlang::ensym(old_arg))

    if (missing(new_arg) && !missing(old_arg)) {
      assign(new_arg_name, old_arg, envir = parent.frame())
      rlang::warn(glue::glue("{old_arg_name} deprecated, use {new_arg_name} instead."),
                  .frequency = .frequency,
                  .frequency_id = old_arg_name)
    }
  }


}
