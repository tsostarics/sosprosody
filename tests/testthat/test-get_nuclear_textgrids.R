test_that("Warning when ouput contains less files", {
  textgrid_df <- data.frame(file = c("a", "a", "c"),
                            word_label = c("ignore", "nucl", "nucl2"))

  expect_warning(get_nuclear_textgrids(textgrid_df, "nucl"),regexp = "1 fewer")

})
