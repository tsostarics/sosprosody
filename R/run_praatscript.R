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
#' If method is set to `system2`, the command will be run using `system2`
#' with `stderr` and `stdout` both set to `TRUE`. The command will be `Praat`
#' and the arguments will be everything following `Praat ` in the system call.
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
#' @param use_gui Logical, defaults to `FALSE` to run the script from the command
#' line using `--run`. When `TRUE`, the script will be run in a new GUI window
#' using `--new-send`. A new line containing `Quit` will be added to the end of
#' the file. This will close the GUI after the script is run; the GUI will not
#' close automatically otherwise and will prevent R from resuming execution.
#' If you want to run a script that creates windows (e.g., `View & Edit`), you
#' must set `use_gui = TRUE`.
#'
#'
#' @return The system call specified by `method` is run, typically `0` if the
#' command was successful
#' @export
run_praatscript <- function(script_path,
                            ...,
                            use_defaults = FALSE,
                            debug = FALSE,
                            praat_path = "Praat",
                            method = base::system,
                            use_gui = FALSE) {
  # Get the arguments as defined by the form in the praat script
  arguments <-
    validate_praat_arguments(script_path, use_defaults, ...) |>
    paste(collapse = " ")

  # If we need to run the script in a new GUI window, we need to create a temp
  # script that includes a Quit command at the end so the GUI can close
  flag <- "--run"
  if (use_gui) {
    temp_script_file <- .make_temp_file(script_path)
    script_path <- temp_script_file
    flag <- "--new-send"
  }

  # Compile the arguments into a single command line string
  systemcall <- paste(praat_path,
                      flag,
                      script_path,
                      arguments)


  if (debug) {
    result <- systemcall
    print(result)
  } else {

    if (identical(method, base::system2)) {
      result <- .use_system2(systemcall)
    } else {
      result <- method(systemcall)
    }

    if (result == 65535){
      .check_for_window_commands(script_path)
     stop("Praat returned an error. Try running the script in Praat or a separate shell window to inspect error message.")
    }
  }

  # Remove the temp file if we made one
  if (use_gui) {
    file.remove(temp_script_file)
  }

  result
}

# Checks for window commands in the script and throws an error if it finds any
.check_for_window_commands <- function(script_path) {
  script <- readLines(script_path)
  window_calls <- grep("(View & Edit)|pauseScript|beginScript", script)
  if (length(window_calls) > 0) {
    stop("This script contains commands that create windows. Use `use_gui = TRUE` to run it.")
  }
}

# base::system and base::system2 format the arguments differently
.use_system2 <- function(systemcall) {
  cmd <- "Praat"
  systemcall <- gsub("^Praat ", "", systemcall)
  base::system2(command = cmd,
                args = systemcall,
                stdout = TRUE,
                stderr = TRUE)
}

.make_temp_file <- function(script_path) {
  # Create a temp script in the same directory as the given script
  temp_dir <- dirname(script_path)
  temp_script_file <- tempfile(tmpdir = temp_dir,fileext = ".praat")

  file.copy(script_path, temp_script_file)

  # Add a new line to temp_script_file that contains just Quit
  # This will close the GUI after the script is run
  cat("\nQuit", file = temp_script_file, sep = "\n", append = TRUE)

  temp_script_file
}
