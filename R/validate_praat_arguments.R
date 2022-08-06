#' Put praatscript arguments in correct order
#'
#' Given a script and named arguments, slot the arguments into the correct
#' position based on where they appear in the script. See `run_praatscript` for
#' more information. Typically you won't need to use this yourself, as it's a
#' helper for `run_praatscript`.
#'
#' @param script_path Path to the script
#' @param use_defaults Logical, whether to fill with default values or not.
#' @param ... *Named* list of arguments to pass to the script. If any character
#' arguments contain spaces, they will be escaped for you.
#'
#' @return
#' @export
validate_praat_arguments <- function(script_path, use_defaults, ...) {
  script_args <- get_praatscript_arguments(script_path)

  # Extract names in order they appear in the form, split will alphabetize list
  script_arg_names <- script_args[['varname']]
  script_args <- split(script_args, ~ varname)

  # Get the arguments provided by the user
  user_args <- rlang::dots_list(...)
  user_arg_names <- names(user_args)
  invalid_args <- !user_arg_names %in% script_arg_names

  if (any(invalid_args))
    stop(glue::glue("Arguments not found in script: {user_arg_names[invalid_args]}"))

  if (!use_defaults && length(user_args) < length(script_args))
    stop("If not using default form arguments, must provide all values to form")

  # Holder for the ordered arguments, to be filled with user or default values
  ordered_args <- script_arg_names[NA]

  for (arg in script_arg_names) {
    arg_value <- user_args[[arg]]

    # If user did not provide this argument
    if (is.null(arg_value)) {
      arg_value <- script_args[[arg]][['default_value']]
      if (is.na(arg_value))
        stop(glue::glue("{arg} does not have a default value in the script, please provide one in ..."))
    }

    # Put the argument value in the correct position
    ordered_args[which(script_arg_names == arg)] <- arg_value
  }

  # Escape any strings that have spaces
  .escape_strings(ordered_args)
}



#' Add quotes to strings for command line call
#'
#' @param strings Character vector
#'
#' @return String vector, any elements containing a space have `"` appended to
#' both sides
.escape_strings <- function(strings) {
  vapply(strings,
         \(string) {
           if (grepl("[[:space:]]", string))
             string <- paste0('"',string,'"')
           string
         },
         "char",
         USE.NAMES = FALSE)
}
