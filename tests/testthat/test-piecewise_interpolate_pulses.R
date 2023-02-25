test_that("piecewise interpolation works", {
  tstdf <- data.frame(tstfile = "f",
                      tstsec = c(rep("later",5),rep("earlier",5)),
                      tsthz = c(seq(10,50,10),
                                c(10,20,30,20,10)),
                      tsttp = c(6:10,
                                1:5))
  int_df <-
    piecewise_interpolate_pulses(tstdf,
                               section_by = "tstsec",
                               pulses_per_section = c("earlier" = 20,
                                                      "later" = 30),
                               time_by = "tsttp",
                               .pitchval = 'tsthz',
                               .grouping = 'tstfile')

  answer_tp <- c(1, 1.211, 1.421, 1.632, 1.842,
                 2.053, 2.263, 2.474, 2.684, 2.895,
                 3.105, 3.316, 3.526, 3.737, 3.947,
                 4.158, 4.368, 4.579, 4.789, 5,
                 6, 6.138, 6.276, 6.414, 6.552,
                 6.69, 6.828, 6.966, 7.103, 7.241,
                 7.379, 7.517, 7.655, 7.793, 7.931,
                 8.069, 8.207, 8.345, 8.483, 8.621,
                 8.759, 8.897, 9.034, 9.172, 9.31,
                 9.448, 9.586, 9.724, 9.862, 10
                 )

  answer_hz <- c(10,
                 12.105, 14.211, 16.316, 18.421, 20.526,
                 22.632, 24.737, 26.842, 28.947, 28.947,
                 26.842, 24.737, 22.632, 20.526, 18.421,
                 16.316, 14.211, 12.105, 10,
                 10, 11.379, 12.759, 14.138, 15.517,
                 16.897, 18.276, 19.655, 21.034, 22.414,
                 23.793, 25.172, 26.552, 27.931, 29.31,
                 30.69, 32.069, 33.448, 34.828, 36.207,
                 37.586, 38.966, 40.345, 41.724, 43.103,
                 44.483, 45.862, 47.241, 48.621, 50)

  expect_equal(round(int_df$tsttp, 3), answer_tp)
  expect_equal(round(int_df$tsthz, 3), answer_hz)
  expect_equal(unique(int_df$tstsec), c('earlier','later'))
  expect_equal(attr(int_df,'groups')$tstfile, "f")

  # plot(tstdf$tsttp, tstdf$tsthz); points(int_df$tsttp, int_df$tsthz, col='red')

})
