test_that("multiplication works", {
  tstdf <- data.frame(tstidx = "base",
    tstfile = c(rep("f1",10), rep("f2",10)),
                      tstsec = c(rep("later",5),rep("earlier",5),
                                 rep("earlier",5), rep("later",5)),
                      tsthz = c(seq(10,50,length.out = 5),
                                seq(50,100, length.out = 5),
                                rev(seq(0,100,length.out = 5)),
                                rev(seq(10,50, length.out = 5))),
                      tsttp = c(6:10,
                                1:5,
                                1:10))

  avg_df <- average_pitchtracks(tstdf,
                      section_by = "tstsec",
                      pulses_per_section = c("earlier" = 10,
                                             "later" = 7),
                      time_by = "tsttp",
                      aggregate_by =  tstfile ~ tstidx,
                      .pitchval = 'tsthz')

  answer_hz <- c(30, 30, 30, 30, 30, 30, 30,
                 75, 650/9, 625/9, 200/3, 575/9,
                 550/9, 175/3, 500/9, 475/9, 50)


  expect_equal(round(avg_df$avg_tsthz,2), round(answer_hz,2))

  # Visual check: Black 10 point line from 75 to 50 and a 7 point line of 30s
  # tstdf |>
  #   ggplot(aes(x = tsttp, y = tsthz, color = tstsec, shape = tstfile, group = tstfile)) +
  #   geom_point() +
  #   geom_point(data = avg_df,
  #              aes(y = avg_tsthz,
  #                  shape = NULL,
  #                  group = NULL),
  #              color = "black",
  #              shape = 4)

})