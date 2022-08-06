#' Run praat script from R
#'
#' Praat scripts can be run from the command line with the command
#' "Praat.exe --run scriptpath.praat arg1 arg2 arg3" etc. where the args
#' are provided in the order of those that appear in the form at the top
#' of the script. This function takes an arbitrary number of ***NAMED arguments***
#' corresponding to the variable names in the praat script and runs the
#' command to run it.
#'
#' Other wrappers like this exist in other packages like `speakr`. However,
#' this version has the benefit of taking the named arguments and ensuring that
#' they're provided to the script *in the correct order* as determined by the
#' order in the script's form.
#'
#' It is important to remember that arguments (such as directories) are
#' evaluated by Praat relative to the script location. Also, this function
#' will not check to ensure that the datatype of the provided argument is
#' compatible with the datatype of the variable in the praatscript, so be
#' careful. Remember that booleans in praat are `1` and `0`, but passing `TRUE`
#' or `FALSE` here will result in passing the the strings `"TRUE"` or `"FALSE"`
#' to the script.
#'
#' On windows (i.e., on my computer), the command only runs if I run
#' `system("Praat --run scriptpath arg1 arg2 arg3")`. `system2` won't work
#' for me, nor will it work if I specify `Praat.exe`. To change these to make
#' it work for you, set `praat_path` to wherever praat is on your computer
#' (honestly it's small enough to just put a fresh copy of the executable in
#' your project directory) and you can change `method` to `base::system2` if
#' needed.
#'
#' See https://www.fon.hum.uva.nl/praat/manual/Scripting_6_9__Calling_from_the_command_line.html
#' for more information on using Praat from the command line.
#'
#' @param script_path Path to the script
#' @param ... *Named* list of arguments to pass to the script. If any character
#' arguments contain spaces, they will be escaped for you in the `system` call.
#' @param use_defaults Logical, defaults to `FALSE`, whether to use the default
#' values for the arguments as found in the form. If the form doesn't have
#' defaults specified, you must provide a value in `...`. If `use_defaults` is
#' set to `TRUE` and an argument is also specified in `...`, the user value will
#' be used instead.
#' @param debug Logical, defaults to `FALSE`, if `TRUE` the system call will not
#' be run and the string that would be passed to it is returned instead. If the
#' command isn't working you can double check that it looks correct with this.
#' @param praat_path Path to Praat.exe, defaults to `Praat` since I keep a copy
#' in my project directories.
#' @param method Which method to use to run system call, defaults to `base::system`,
#' but you could also use `base::system2` or `I` to return the string itself
#'
#' @return The system call specified by `method` is run, typically `0` if the
#' command was successful
#' @export
run_praatscript <- function(script_path,
                            ...,
                            use_defaults = FALSE,
                            debug = FALSE,
                            praat_path = "Praat",
                            method = base::system) {
  arguments <-
    validate_praat_arguments(script_path, use_defaults, ...) |>
    paste(collapse = " ")

  systemcall <- paste(praat_path,
                      "--run",
                      script_path,
                      arguments)
  if (debug) {
    print(systemcall)
  } else {
    method(systemcall)
  }
}
