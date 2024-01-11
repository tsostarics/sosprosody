test_that("averaging two files with two sections works (ie a simple case)",{
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
                                .pitchval = 'tsthz',.sort = TRUE)


  answer_hz <- c(75, 650/9, 625/9, 200/3, 575/9,
                 550/9, 175/3, 500/9, 475/9, 50,
                 30, 30, 30, 30, 30, 30, 30)


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

test_that("errors for bad formulas work", {
  # The actual dataframe here doesnt matter since these errors happen early on
  expect_error(average_pitchtracks(mtcars, section_by = 'cyl', aggregate_by = ~ gear),
               regexp= "must be two")
  expect_error(average_pitchtracks(mtcars, section_by = 'cyl', aggregate_by = cyl+gear ~ gear),
               regexp="LHS must contain only one")
})

