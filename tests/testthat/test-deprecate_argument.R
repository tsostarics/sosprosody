test_that("warning when deprecated parameter used", {
  foo <- function(x, y) {
    deprecate_argument(x, y, 'always')

    x
  }

  expect_warning(foo(y=4), "y deprecated, use x instead.")
})
