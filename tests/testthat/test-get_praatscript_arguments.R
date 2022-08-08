test_that("praatscript argument extraction works", {

  script_path <- local_praatscript()
  arg_df <- get_praatscript_arguments(script_path)

  expect_equal(arg_df$datatype,
               c('text','text','integer','real','real','optionmenu'))
  expect_equal(arg_df$varname,
               c('fromDir','outDir','val1','val2','val3','measure'))
  expect_equal(arg_df$default_value,
               c("C:/", NA, NA, NA, "0.2","this one"))

})
