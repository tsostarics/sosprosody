test_that("interpolate equal pulses works", {
  tstdf <- data.frame(tstfile = c(rep("a",5),rep("b",5)),
                      tsthz = c(seq(10,50,10),
                                c(10,20,30,20,10)),
                      tsttp = c(6:10,
                                1:5))

  tst2 <- tstdf
  tst2$tsttp[1:2] <- 7
  tst2$tsthz[1:2] <- 20

  int_df <- interpolate_equal_pulses(tstdf,
                                     n_pulses = 10,
                                     x = 'tsttp',
                                     y = 'tsthz',
                                     group = 'tstfile')


  answer_df <- data.frame(tstfile = c(rep("a",10),rep("b",10)),
                          tsttp = round(c(seq(6,10,length.out = 10),
                                          seq(1,5,length.out = 10)),2),
                          tsthz = c(10, 14.44, 18.89, 23.33, 27.78,
                                    32.22, 36.67, 41.11, 45.56, 50,
                                    10, 14.44, 18.89, 23.33, 27.78,
                                    27.78, 23.33, 18.89, 14.44, 10))

  expect_warning(interpolate_equal_pulses(tst2, 10, 'tsttp', 'tsthz','tstfile'))
  expect_equal(round(int_df$tsttp,2), answer_df$tsttp)
  expect_equal(round(int_df$tsthz,2), answer_df$tsthz)
  expect_equal(int_df$tstfile, answer_df$tstfile)
  # expect_equal(attr(int_df,'groups')$tstfile, c('a','b'))
  # Check by visualization, the red points should be interpolated bw the black:
  # plot(tstdf$tsttp, tstdf$tsthz); points(int_df$tsttp, int_df$tsthz, col='red')

})

test_that("error when mismatching lengths with interpolation",
          {
expect_error(interpolate_pitchpoints(seq(6,9,20), 6:9, 6:10),regexp = "must have equal")
          }

)

test_that("error when value exists outside of range",
          {
            expect_error(interpolate_pitchpoints(rep(6,6), rep(7,7), rep(7,7)),regexp = "inclusive range")
            expect_error(interpolate_pitchpoints(rep(8,8), rep(7,7), rep(7,7)),regexp = "inclusive range")
          }

)

test_that("all NaNs when input only contains duplicate pulse values", {
  expect_equal(interpolate_pitchpoints(rep(7,3), rep(7,3), rep(7,3)), c(NaN, NaN, NaN))
})

test_that("one NaN when starting with a duplicate", {
  expect_equal(interpolate_pitchpoints(6:9, c(6,6,8,9), c(10,10,30,40)),
               c(NaN, 20, 30, 40))
})

