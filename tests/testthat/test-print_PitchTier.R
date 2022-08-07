test_that("print pitchtier snapshot matches", {
  expect_snapshot(print(new_pitchtier()))
})
