test_that("time normalization without from zero shifting works", {
  test_df <- data.frame(file = "test",
                        timepoint = seq(.1,2,by = .1))

  normed_df <- time_normalize(test_df, .fromzero = FALSE)

  expect_equal(normed_df$timepoint_norm, seq(.05,1,by = .05))
})

test_that("from zero shifting works", {
  test_df <- data.frame(file = c(rep("a", 4), rep("b", 4)),
                        timepoint = c(seq(.1, .4, by = .1),
                                      seq(1.1, 1.4, by = .1)))

  normed_df <- time_normalize(test_df)

  expect_equal(normed_df$timepoint_norm, c(0, 1/3, 2/3, 1, 0, 1/3, 2/3, 1))
})

test_that("default names work", {
  test_df <- data.frame(file = "test",
                        timepoint = c(1, 2))

  normed_df <- time_normalize(test_df)

  expect_equal(names(normed_df), c("file", "timepoint", "timepoint_norm"))
})

test_that("overwriting with .to works", {
  test_df <- data.frame(file = "test",
                        timepoint = c(1, 2))

  normed_df <- time_normalize(test_df, .to = "timepoint")

  expect_equal(names(normed_df), c("file", "timepoint"))
})

test_that("error when missing .from works", {
  test_df <- data.frame(file = "test",
                        timepoint = c(1, 2))

  expect_error(time_normalize(test_df, .from = 'time'),
               regexp = "`time` not found in pitchtier")
})

test_that("nondefault .to works", {
  test_df <- data.frame(file = "test",
                        timepoint = c(1, 2))
  new_name = "t"
  normed_df <- time_normalize(test_df, .to = new_name)

  expect_equal(names(normed_df), c("file", "timepoint", "t"))
})

