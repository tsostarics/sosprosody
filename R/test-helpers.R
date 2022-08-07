local_praatscript <- function(..., env = parent.frame()) {

  file.create("testscript.praat")

  writeLines(c("form",
               "\tcomment Test line one",
               "\ttext fromDir C:/",
               "\ttext outDir",
               "\tinteger val1 ",
               "\treal val2",
               "\treal val3 0.2",
               "endform"),
             con = "testscript.praat")

  withr::defer(
    file.remove("testscript.praat"),
    envir = env
  )

  "testscript.praat"
}

new_pitchtier <- function() {
  times <- seq(0,1.6, length.out = 100)
  frequencies <- c(seq(60,90, length.out = 30),
                   seq(90,60, length.out = 30),
                   seq(60, 100, length.out = 40))

  pitchtier <-
    list(
      t = times,
      f = frequencies,
      tmin= 0,
      tmax = 1.6
    )

  attr(pitchtier, 'names') <- c('t', 'f', 'tmin', 'tmax')
  attr(pitchtier, 'class') <- c("list", type = "PitchTier", name = "testpt.PitchTier")

  pitchtier
}

new_textgrid <- function() {
  interval_tier <-
    list(
      name = "words",
      type = "interval",
      t1 = c(0, .1, .5, .7, 1.4),
      t2 = c(.1, .5, .6, 1.4, 2),
      label = c("", "a", "b", "c", "d", "")
    )

  point_tier <-
    list(
      name = "tones",
      type = "point",
      t = c(.1, .5, .6, 1.6),
      label = c("", "a", "b", "c", "d")
    )

  textgrid <-
    list(
      words = interval_tier,
      tones = point_tier
    )

  attr(textgrid, "names") <- c("words", "tones")
  attr(textgrid, "class") <- c("list",
                               tmin = 0,
                               tmax = 2,
                               type = "TextGrid",
                               name = "tst.TextGrid")

  textgrid
}
