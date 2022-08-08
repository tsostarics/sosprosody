test_that("conversion to textgrid from tier list works", {
  tg <- as_textgrid(unclass(new_textgrid()),filename = "tst.TextGrid")

  expect_equal(new_textgrid(), tg)
})
