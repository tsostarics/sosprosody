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
  optionmenu <- list()

  for (i in seq_along(lines)) {
    if (grepl("^form", lines[i]))
      form_line <- i
    if (grepl("^endform", lines[i]))
      endform_line <- i
    if (grepl("^[[:space:]]*optionmenu", lines[i])) {
      # TODO: make this a helper because it's obnoxious
      menu_options <- c()
      menu_name <- stringr::str_match(lines[i], "optionmenu ([^:]+)")[2]
      search_menu_options <- TRUE
      j = i + 1
      while (search_menu_options) {
        option_name <- stringr::str_match(lines[j],"option (.+)$")[2]
        menu_options <- c(menu_options, option_name)
        j = j+1
        search_menu_options <- grepl("option", lines[j])
      }

      optionmenu <-
        list(
          optionmenu,
          list(menu_name = menu_name,
               menu_options = menu_options)
        )
    }
    if (grepl("^[[:space:]]*option ", lines[i]))
      next
    if (endform_line > 0)
      break
  }

  if (form_line == endform_line)
    stop("No form found in script")

  optionmenu <- optionmenu[-1]
  names(optionmenu) <- vapply(optionmenu, \(x) x[['menu_name']], 'char')
  # Extract the arguments from the form
  # Regex: any amount of space, (datatype), space, (varname), space, (default)
  # Note that with optionmenu we need to avoid an extra : when getting varname
  arguments <-
    stringr::str_match_all(trimws(lines[seq(form_line + 1, endform_line - 1)]),
                           "([^ ]+) ([^ :]+):? ?(.+)?$") |>
    lapply(\(args)
           args[-1])

  # Catches any blank lines
  arguments <- arguments[vapply(arguments,
                                \(x) !identical(x,character(0)),TRUE)]

  # Filter out any comments
  arguments <- arguments[vapply(arguments,
                                \(x) !x[1] %in% c('comment', 'option'),TRUE)]

  # Set up the dataframe with datatypes, varnames, and default values
  arg_df <-
    data.frame(datatype = vapply(arguments, \(x) x[1],"char"),
             varname = vapply(arguments, \(x) x[2], "char"),
             default_value = vapply(arguments,
                                    \(x) ifelse(length(x) == 3,
                                                x[3],
                                                NA_character_),
                                    "char")
  )

  # Option menus are dumb and require shell scripts to pass the full string
  # of the selected option instead of the numeric index, so we need to change
  # the default values accordingly by looking them up in the menus we made earlier
  which_are_optionmenu <- arg_df[['datatype']] == 'optionmenu'
  default_options <- vapply(arg_df$varname[which_are_optionmenu],
                            \(which_menu) {
                              menu_options <- optionmenu[[which_menu]][["menu_options"]]
                              def_val <- arg_df[arg_df$varname == which_menu,'default_value']
                              menu_options[as.integer(def_val)]
                            },
                            "char", USE.NAMES = FALSE)

  arg_df[which_are_optionmenu,"default_value"] <- default_options

  arg_df
}
