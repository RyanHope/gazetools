#' Detect Blinks (static window)
#'
#' Detects blinks based on pupil status/valicity
#' 
#' @param pupil a vector of class \code{numeric}; the status/validity of the pupil
#' @param samplerate the samplerate of the eyetracker
#' @param pad time in ms to pad each side of each no pupil segment
#'
#' @return a vector of class \code{logical}; indicates samples that belong to a blink
#' 
#' @export
#' 
#' @examples
#' data(lc120)
#' d.b <- with(lc120, detect_blinks.SW(status, 120))
#' str(d.b)
#' 
detect_blinks.SW <- function(pupil, samplerate, pad=50) {
  pad <- ceiling((pad/1000)/(1/samplerate))
  l <- length(pupil)
  d <- rep(F,l)
  r <- find_peak_ranges(!pupil,T)
  if (!is.null(r)) {
    for (i in 1:nrow(r)) {
      begin <- r[i,"begin"]
      end <- r[i,"end"]
      if (begin-pad > 0)
        begin <- begin - pad
      else
        begin <- 0
      if (end+pad > l)
        end <- l
      else
        end <- end + pad
      d[begin:end] <- T
    }
  }
  d
}