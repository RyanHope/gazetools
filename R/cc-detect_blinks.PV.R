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
#' @export
#' 
#' @examples
#' data(smi)
#' d.b <- with(smi, detect_blinks.PV(smi_dyl, 500))
#' str(d.b)
#' 
detect_blinks.PV <- function(pupil, samplerate, wl = 51, pvt = 1000) {
  ts <- 1 / samplerate
  d <- data.frame(x=1:length(pupil),pupil=pupil)
  d$pupil_smoothed <- lowess(d$pupil,f=.001)$y
  d$v <- lowess(abs(sgolayfilt(d$pupil_smoothed, n = wl, p = 2, m = 1, ts = ts)),f=.01)$y
  d$v > pvt
}