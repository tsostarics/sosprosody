utils::globalVariables(c("prob", "section", "y"))

.onLoad <- function(libname, pkgname) {
  op <- options()

  # If reframe doesn't exist, catch error and select summarize instead
  fx <- tryCatch(dplyr::reframe, error = \(e) dplyr::summarize)


  op.sosprosody <- list(
    sosprosody.reframe_fx = fx
  )


  toset <- !(names(op.sosprosody) %in% names(op))
  if (any(toset)) options(op.sosprosody[toset])

  invisible()
}
