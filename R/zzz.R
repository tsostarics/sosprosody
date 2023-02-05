utils::globalVariables(c("prob", "section", "y"))

.onLoad <- function(libname, pkgname) {
  op <- options()
  dplyr_version <- as.numeric(gsub("\\.","",
                                   utils::packageVersion('dplyr'),
                                   perl = TRUE))

  op.sosprosody <- list(
    sosprosody.reframe_fx = ifelse(dplyr_version <10999000,
                                   \(x) dplyr::summarize(.groups = "drop"), dplyr::reframe)
  )


  toset <- !(names(op.sosprosody) %in% names(op))
  if (any(toset)) options(op.sosprosody[toset])

  invisible()
}
