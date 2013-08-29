#' Detect Blinks
#'
#' Detects blinks based on pupil diameter velocity
#' 
#' @param pupil a vector of class \code{numeric}; the vertical diameter of the pupil
#' @param samplerate the samplerate of the eyetracker
#' @param window window length of smoothing filter
#' @param threshold pupil diameter velocity threhold
#'
#' @return a vector of class \code{logical}; indicates samples that belong to a blink
#'
#' @importFrom signal sgolayfilt
#' 
#' @rdname detect_blinks
#' 
#' @export
#' 
#' @example example/detect_blinks.R
#' 
detect_blinks <- function(pupil, samplerate, window = 51, threshold = 1000) {
  ts <- 1 / samplerate
  d <- data.frame(x=1:length(pupil),pupil=pupil)
  d$pupil_smoothed <- lowess(d$pupil,f=.001)$y
  d$v <- lowess(abs(sgolayfilt(d$pupil_smoothed, n = window, p = 2, m = 1, ts = ts)),f=.01)$y
  d$v > threshold
}