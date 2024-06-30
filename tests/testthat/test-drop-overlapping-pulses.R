test_that("juncture handling works", {
  tstdf <- data.frame(tstfile = "f",
                      tstsec = c(rep("later",6),rep("earlier",5)),
                      tsthz = c(seq(10,50,length.out = 6),
                                c(10,20,30,20,10)),
                      tsttp = c(5:10,
                                1:5))
  int_df <-
    piecewise_interpolate_pulses(tstdf,
                                 section_by = "tstsec",
                                 pulses_per_section = c("earlier" = 20,
                                                        "later" = 30),
                                 x = "tsttp",
                                 y = 'tsthz',
                                 group = 'tstfile',
                                 sort_first = TRUE,
                                 index_by = NULL)

  adjusted_df_l <- drop_overlapping_pulses(int_df,'l','tsttp','pulse_i',group = 'tstfile')
  adjusted_df_r <- drop_overlapping_pulses(int_df,'r','tsttp','pulse_i',group = 'tstfile')
  adjusted_df_b <- drop_overlapping_pulses(int_df,'b','tsttp','pulse_i',group = 'tstfile')

  expect_equal(as.vector(xtabs(~tstsec,data =  adjusted_df_l)),
               c(20,29))

  expect_equal(as.vector(xtabs(~tstsec,data =  adjusted_df_r)),
               c(19,30))

  expect_equal(adjusted_df_b, int_df)


})
