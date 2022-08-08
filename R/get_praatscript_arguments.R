#' Extract arguments from praat script form
#'
#' Arguments in a praat script are specified in the form. This is the top-most
#' portion of a script that starts with `form` and ends with `endform`.
#' Between these, the argument names and types are declared, along with
#' an optional default value. This function will read the form section of the
#' script and return a dataframe containing the form arguments, their datatype,
#' and their default value. Lines with comments will be removed.
#' All values will be coerced to the `character` R datatype.
#' If a default value is not found, `NA` will be returned.
#' The order of the variables is shown in the order they appear in the script.
#'
#' See https://www.fon.hum.uva.nl/praat/manual/Scripting_6_1__Arguments_to_the_script.html
#' for more information on Praat data types.
#'
#' @param script_path Path to the script
#'
#' @return Dataframe with `datatype`, `varname`, and `default_value` columns
#' @export
get_praatscript_arguments <- function(script_path) {
  # Read in praat script
  lines <- suppressWarnings(readLines(script_path))

  # Determine where the form is in the script
  form_line <- 0
  endform_line <- 0
  for (i in seq_along(lines)) {
    if (grepl("^form", lines[i]))
      form_line <- i
    if (grepl("^endform", lines[i]))
      endform_line <- i
    if (endform_line > 0)
      break
  }

  if (form_line == endform_line)
    stop("No form found in script")

  # Extract the arguments from the form
  arguments <-
    stringr::str_match_all(lines[seq(form_line + 1, endform_line - 1)],
                           "[[:space:]]*([^ ]+) ([^ ]+) ?(.+)?$") |>
    lapply(\(args)
           args[-1])

  # Catches any blank lines
  arguments <- arguments[vapply(arguments,\(x) !identical(x,character(0)),TRUE)]

  # Filter out any comments
  arguments <- arguments[vapply(arguments, \(x) x[1] != 'comment',TRUE)]

  data.frame(datatype = vapply(arguments, \(x) x[1],"char"),
             varname = vapply(arguments, \(x) x[2], "char"),
             default_value = vapply(arguments,
                                    \(x) ifelse(length(x) == 3,
                                                x[3],
                                                NA_character_),
                                    "char")
  )

}
