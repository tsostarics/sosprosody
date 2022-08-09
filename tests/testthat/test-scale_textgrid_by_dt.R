test_that("simple 1-point duration warping works", {
  dt_obj <- new_pitchtier()
  dt_obj[['t']] <- 1
  dt_obj[['f']] <- 1/3
  warped <- scale_textgrid_by_dt(new_textgrid(), dt_obj)

  # Check interval calculations
  expect_equal(warped[[1]][['t2']],
               c(1/30, 1/6, 7/30, 7/15, 2/3))

  # Check point calculations
  expect_equal(warped[[2]][['t']],
               c(1/30, 1/6, 1/5, 8/15))
})
