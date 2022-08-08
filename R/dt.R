#' dt.read
#'
#' Reads DurationTier from Praat. Supported formats: text file, short text file,
#' spreadsheet, headerless spreadsheet (headerless not recommended,
#' it does not contain tmin and tmax info).
#'
#' Note: All credit for these functions go to Bořil, T., & Skarnitzl, R. (2016)
#' I needed to read in duration tiers, which have the same exact format as
#' pitch tiers, so all I really needed was to find-and-replace "PitchTier" with
#' "DurationTier" from here:
#' https://github.com/bbTomas/rPraat/blob/master/R/pt.R
#'
#' @param fileNameDurationTier file name of DurationTier
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
#'
#' @return DurationTier object
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- dt.read("demo/H.DurationTier")
#' }
dt.read <- function(fileNameDurationTier, encoding = "UTF-8") {
  if (!rPraat::isString(fileNameDurationTier)) {
    stop("Invalid 'fileNameDurationTier' parameter.")
  }

  if (!rPraat::isString(encoding)) {
    stop("Invalid 'encoding' parameter.")
  }
  enc <- encoding

  if (encoding == "auto") {
    enc <- rPraat::detectEncoding (fileNameDurationTier)
  }

  if (enc == "UTF-8") {
    flines <- readr::read_lines(fileNameDurationTier, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
  } else {
    fid <- file(fileNameDurationTier, open = "r", encoding = enc)
    flines <- readLines(fid)   # does not work with tests/testthat/utf8.TextGrid  :-(
    close(fid)
  }

  flines <- enc2utf8(flines)


  if (length(flines) < 1) {
    stop("Empty file.")
  }

  if (encoding == "UTF-8") {
    if (flines[1] != 'File type = "ooTextFile"'  &  flines[1] != '"ooTextFile"') {
      # maybe headerlessSpreadSheet ?
      spl <- strsplit(flines[1], "\t")
      if (length(spl[[1]]) != 2) {
        warning('Not an UTF-8 DurationTier format, trying encoding = "auto"...')
        x <- dt.read(fileNameDurationTier, encoding = "auto")
        return(x)
      }
    }
  }

  pt_ind <- dt.read_lines(flines)
  class(pt_ind[[1]])["type"] <- "DurationTier"
  class(pt_ind[[1]])["name"] <- basename(fileNameDurationTier)
  return(pt_ind[[1]])
}

dt.read_lines <- function(flines, find = 1, collection = FALSE) {
  if (flines[find-1+1] == "\"ooTextFile\"") {    # spreadSheet - cannot be in collection file
    if (collection) {
      stop("unsupported DurationTier format (SpreadSheet) in collection")
    }

    if (length(flines)-find+1 < 3) {
      stop("Unknown DurationTier format.")
    }

    if (flines[find-1+2] != "\"DurationTier\"") {
      stop("Unknown DurationTier format.")
    }

    fromToN <- stringr::str_split(flines[find-1+3], " ")
    if (length(fromToN[[1]]) != 3) {
      stop("Unknown DurationTier format.")
    }
    xmin <- as.numeric(fromToN[[1]][[1]])
    xmax <- as.numeric(fromToN[[1]][[2]])
    N <- as.integer(fromToN[[1]][[3]])

    if (N != (length(flines)-3)) {
      stop("Wrong number of points in DurationTier format.")
    }
    t <- numeric(N)
    f <- numeric(N)

    for (I in rPraat::seqM(1, N, by = 1)) {
      tf <- stringr::str_split(flines[find-1+I+3], "\\s")
      if (length(tf[[1]]) != 2) {
        stop("Unknown DurationTier format.")
      }
      t[I] <- as.numeric(tf[[1]][[1]])
      f[I] <- as.numeric(tf[[1]][[2]])
    }


  } else if (collection  ||  flines[find-1+1] == "File type = \"ooTextFile\"") {    # TextFile or shortTextFile - only these 2 formats can be stored in collection file
    if (!collection) {
      if (length(flines)-find+1 < 6) {
        stop("Unknown DurationTier format.")
      }

      if (rPraat::strTrim(flines[find-1+2]) != "Object class = \"DurationTier\"") {
        stop("Unknown DurationTier format.")
      }

      if (rPraat::strTrim(flines[find-1+3]) != "") {
        stop("Unknown DurationTier format.")
      }

      if (rPraat::strTrim(nchar(flines[find-1+4])) < 1) {
        stop("Unknown DurationTier format.")
      }
    } else {
      find <- find - 3
    }

    if (rPraat::str_contains(flines[find-1+4], "xmin")) {  # TextFile
      xmin <- as.numeric(substr(rPraat::strTrim(flines[find-1+4]), 8,  nchar(rPraat::strTrim(flines[find-1+4]))))
      xmax <- as.numeric(substr(rPraat::strTrim(flines[find-1+5]), 8,  nchar(rPraat::strTrim(flines[find-1+5]))))
      N <-    as.numeric(substr(rPraat::strTrim(flines[find-1+6]), 16, nchar(rPraat::strTrim(flines[find-1+6]))))

      # if (N != (length(flines)-6)/3) {
      #     stop("Wrong number of points in DurationTier format.")
      # }
      t <- numeric(N)
      f <- numeric(N)

      for (I in rPraat::seqM(1, N, by = 1)) {
        t[I] <- as.numeric(substr(rPraat::strTrim(flines[find-1+8 + (I-1)*3]), 10, nchar(rPraat::strTrim(flines[find-1+8 + (I-1)*3]))))
        f[I] <- as.numeric(substr(rPraat::strTrim(flines[find-1+9 + (I-1)*3]), 9, nchar(rPraat::strTrim(flines[find-1+9 + (I-1)*3]))))
      }

      find <- find-1+9 + (N-1)*3 + 1

    } else {   # shortTextFile

      xmin <- as.numeric(flines[find-1+4])
      xmax <- as.numeric(flines[find-1+5])
      N <- as.integer(flines[find-1+6])

      # if (N != (length(flines)-6)/2) {
      #     stop("Wrong number of points in DurationTier format.")
      # }
      t <- numeric(N)
      f <- numeric(N)

      for (I in rPraat::seqM(1, N, by = 1)) {
        t[I] <- as.numeric(flines[find-1+7 + (I-1)*2])
        f[I] <- as.numeric(flines[find-1+8 + (I-1)*2])
      }

      find <- find-1+8 + (N-1)*2 + 1
    }


  } else {   # headerless SpreadSheet - cannot be in collection file
    if (collection) {
      stop("unsupported DurationTier format (headerless SpreadSheet) in collection")
    }

    N <- length(flines)

    t <- numeric(N)
    f <- numeric(N)

    for (I in rPraat::seqM(1, N, by = 1)) {
      tf <- stringr::str_split(flines[I], "\\s")
      if (length(tf[[1]]) != 2) {
        stop("Unknown DurationTier format.")
      }
      t[I] <- as.numeric(tf[[1]][[1]])
      f[I] <- as.numeric(tf[[1]][[2]])
    }

    xmin <- min(t)
    xmax <- max(t)
  }


  pt <- list(t = t, f = f, tmin = xmin, tmax = xmax)

  return(list(pt, find))
}


#' dt.write
#'
#' Saves DurationTier to a file (in UTF-8 encoding).
#' \code{pt} is a list with \code{$t} and \code{$f} vectors (of the same length) at least.
#' If there are no \code{$tmin} and \code{$tmax} values, there are
#' set as min and max of \code{$t} vector.
#'
#' Note: All credit for these functions go to Bořil, T., & Skarnitzl, R. (2016)
#' I needed to read in duration tiers, which have the same exact format as
#' pitch tiers, so all I really needed was to find-and-replace "PitchTier" with
#' "DurationTier" from here:
#' https://github.com/bbTomas/rPraat/blob/master/R/pt.R
#'
#' @param pt DurationTier object
#' @param fileNameDurationTier file name to be created
#' @param format Output file format (\code{"short"} (short text format), \code{"text"} (a.k.a. full text format), \code{"spreadsheet"} (default), \code{"headerless"} (not recommended, it does not contain \code{tmin} and \code{tmax} info))
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pt <- dt.sample()
#' pt <- dt.Hz2ST(pt)    #  conversion of Hz to Semitones, reference 0 ST = 100 Hz.
#' dt.plot(pt)
#' dt.write(pt, "demo/H_st.DurationTier")
#' }
dt.write <- function(pt, fileNameDurationTier, format = "spreadsheet") {
  dt.write0(pt, fileNameDurationTier, format)
}

dt.write0 <- function(pt, fileNameDurationTier, format = "spreadsheet", fid = NULL, collection = FALSE) {
  if (!isString(fileNameDurationTier)) {
    stop("Invalid 'fileNameDurationTier' parameter.")
  }

  if (!isString(format)) {
    stop("Invalid 'format' parameter.")
  }

  if (format != "short" && format != "text" && format != "spreadsheet" && format != "headerless") {
    stop("Unsupported format (supported: short, text, spreadsheet [default], headerless)")
  }

  if (!("t" %in% names(pt))) {
    stop("pt must be a list with 't' and 'f' and optionally 'tmin' and 'tmax'")
  }
  if (!("f" %in% names(pt))) {
    stop("pt must be a list with 't' and 'f' and optionally 'tmin' and 'tmax'")
  }
  if (length(pt$t) != length(pt$f)) {
    stop("t and f lengths mismatched.")
  }
  N <- length(pt$t)


  if (!("tmin" %in% names(pt))) {
    xmin <- min(pt$t)
  } else {
    xmin <- pt$tmin
  }
  if (!("tmax" %in% names(pt))) {
    xmax <- max(pt$t)
  } else {
    xmax <- pt$tmax
  }


  if (!collection) {
    fid <- file(fileNameDurationTier, open = "wb", encoding = "UTF-8")
    if (!isOpen(fid)) {
      stop(paste0("cannot open file [", fileNameDurationTier, "]"))
    }
  }

  if (!collection) {
    if (format == "spreadsheet") {
      wrLine('"ooTextFile"', fid)
      wrLine('"DurationTier"', fid)
    } else if (format == "short" || format == "text") {
      wrLine('File type = "ooTextFile"', fid)
      wrLine('Object class = "DurationTier"', fid)
      wrLine('', fid)
    }
  }

  if (format == "spreadsheet") {
    wrLine(paste0(as.character(round2(xmin, -15)), " ", as.character(round2(xmax, -15)), " ", as.character(N)), fid)
  } else if (format == "short") {
    wrLine(as.character(round2(xmin, -15)), fid)
    wrLine(as.character(round2(xmax, -15)), fid)
    wrLine(as.character(N), fid)
  } else if (format == "text") {
    wrLine(paste0("xmin = ", as.character(round2(xmin, -15)), " "), fid, collection)
    wrLine(paste0("xmax = ", as.character(round2(xmax, -15)), " "), fid, collection)
    wrLine(paste0("points: size = ", as.character(N), " "), fid, collection)
  }

  for (n in seqM(1, N)) {
    if (format == "spreadsheet" || format == "headerless") {
      wrLine(paste0(as.character(round2(pt$t[n], -15)), "\t", as.character(round2(pt$f[n], -15))), fid)
    } else if (format == "short") {
      wrLine(as.character(round2(pt$t[n], -15)), fid)
      wrLine(as.character(round2(pt$f[n], -15)), fid)
    } else if (format == "text") {
      wrLine(paste0("points [", as.character(n), "]:"), fid, collection)
      wrLine(paste0("    number = ", as.character(round2(pt$t[n], -15)), " "), fid, collection)
      wrLine(paste0("    value = ", as.character(round2(pt$f[n], -15)), " "), fid, collection)
    }
  }

  if (!collection) {
    close(fid)
  }
}
