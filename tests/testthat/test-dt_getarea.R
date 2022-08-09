test_that("area-from-1 calculations work", {
  dt_obj <- new_pitchtier()
  dt_obj[['t']] <- c(1, 2, 3, 4, 5, 6)
  dt_obj[['f']] <- c(1/3, 1/3, 1, 3, 1, 2)

  expect_equal(dt_getarea(dt_obj, 1, 2), -2/3)
  expect_equal(dt_getarea(dt_obj,0,1), -2/3)
  expect_equal(dt_getarea(dt_obj,6,7), 1)
  expect_equal(dt_getarea(dt_obj, 3, 5), 2)
  expect_equal(dt_getarea(dt_obj, 1, 6), 3/2)
})
