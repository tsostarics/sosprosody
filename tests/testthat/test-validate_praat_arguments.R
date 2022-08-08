test_that("string escaping works", {
  expect_equal(.escape_strings(c("a", "b c", " d", " e ")),
               c('a', '"b c"', '" d"', '" e "'))
  expect_equal(.escape_strings("^$|^\\s+$|r"),
               '"^$|^\\s+$|r"')
})

test_that("fully specified arguments, same order, validated", {
  script_path <- local_praatscript()

  user_args <-
    validate_praat_arguments(script_path,
                             use_defaults = TRUE,
                             fromDir = "D:/",
                             outDir = "D:/A",
                             val1 = 2,
                             val2 = 3,
                             val3 = 0.2,
                             measure = "this one")

  expect_equal(user_args, c("D:/", "D:/A", "2", "3", "0.2", '"this one"'))
})

test_that("fully specified arguments, different order, validated", {
  script_path <- local_praatscript()

  user_args <-
    validate_praat_arguments(script_path,
                             use_defaults = TRUE,
                             val1 = 2,
                             fromDir = "D:/",
                             outDir = "D:/A",
                             val3 = 0.2,
                             measure = 3,
                             val2 = "3")

  expect_equal(user_args, c("D:/", "D:/A", "2", "3", "0.2", "3"))
})

test_that("no arguments specified, errors with defaults", {
  script_path <- local_praatscript()

  expect_error(
    validate_praat_arguments(script_path,
                             use_defaults = TRUE),
    regexp = "not have a default value"
  )
})

test_that("no arguments specified, errors without defaults", {
  script_path <- local_praatscript()

  expect_error(
    validate_praat_arguments(script_path,
                             use_defaults = FALSE),
    regexp = "not using default"
  )

})

test_that("partially specified arguments, fill defaults", {
  script_path <- local_praatscript()

  user_args <-
    validate_praat_arguments(script_path,
                             use_defaults = TRUE,
                             outDir = "D:/A",
                             val1 = 2,
                             val2 = "3")

  expect_equal(user_args, c("C:/", "D:/A", "2", "3", "0.2",'"this one"'))
})

test_that("error on unknown argument", {
  script_path <- local_praatscript()

  expect_error(
    validate_praat_arguments(script_path,
                             use_defaults = TRUE,
                             val4 = 2),
    regexp = "not found in script: val4"
  )
})
