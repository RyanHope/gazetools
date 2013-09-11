#' Detect Blinks (pupil velocity)
#'
#' Detects blinks based on pupil diameter velocity
#' 
#' @param pupil a vector of class \code{numeric}; the vertical diameter of the pupil
#' @param samplerate the samplerate of the eyetracker
#' @param wl window length of smoothing filter
#' @param pvt pupil diameter velocity threhold
#'
#' @return a vector of class \code{logical}; indicates samples that belong to a blink
#'
#' @importFrom signal sgolayfilt
#' 
#' @rdname detect_blinks.PV
#' 
#' @export
#' 
#' @example example/detect_blinks.R
#' 
detect_blinks.PV <- function(pupil, samplerate, wl = 51, pvt = 1000) {
  ts <- 1 / samplerate
  d <- data.frame(x=1:length(pupil),pupil=pupil)
  d$pupil_smoothed <- lowess(d$pupil,f=.001)$y
  d$v <- lowess(abs(sgolayfilt(d$pupil_smoothed, n = wl, p = 2, m = 1, ts = ts)),f=.01)$y
  d$v > pvt
}

#' Detect Blinks (static window)
#'
#' Detects blinks based on pupil status/valicity
#' 
#' @param pupil a vector of class \code{numeric}; the status/validity of the pupil
#' @param samplerate the samplerate of the eyetracker
#' @param pad time in ms to pad each side of a no pupil signal
#'
#' @return a vector of class \code{logical}; indicates samples that belong to a blink
#' 
#' @rdname detect_blinks.SW
#' 
#' @export
#' 
#' @example example/detect_blinks.R
#' 
detect_blinks.SW <- function(status, samplerate, pad=50) {
  pad <- ceiling((pad/1000)/(1/samplerate))
  l <- length(status)
  d <- rep(F,l)
  r <- find_peak_ranges(!status,T)
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