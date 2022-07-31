test_that("Error when missing file in nuclear regions", {
  pitchtier_df <- data.frame(file = c("a", "a", "c"),
                             timepoint = c("1.5", "1.7", "1.6"))

  nuclear_df <- data.frame(file = "a",
                           word_start = "1",
                           word_end = "2")

  expect_warning(code_nuclear_pulses(pitchtier_df, nuclear_df),regexp = "Removing 1 files")
})
