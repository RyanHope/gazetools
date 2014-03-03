#' Compute Velocity and Acceleration Profiles for Raw Gaze Data
#'
#' Takes standard eye tracker gaze information and computes smoothed position, velocity
#' and acceleration profiles using a Savitzky-Golay filter.
#' 
#' @template 1p
#' @param samplerate the samplerate of the eyetracker
#' @template eye
#' @template sg
#' @param pupil a vector of class \code{numeric}; the vertical diameter of the pupil or a pupil status signal
#' @param blinkFUN the name of the blink detection algorithm to use
#' @param ... arguments to be passed to blinkFUN
#'
#' @return an object of class \code{\link[=pva-class]{pva}}
#'
#' @importFrom zoo na.approx
#' @importFrom signal sgolayfilt
#' 
#' @rdname pva
#' 
#' @export
#' 
#' @example examples/pva.R
#'  
pva <- function(x, y, samplerate, rx, ry, sw, sh, ez,
                    ex = 0, ey = 0, order = 2, window = 11,
                pupil = NULL, blinkFUN = "detect_blinks.PV", ...)
{
  N <- nrow(data.frame(x=x,y=y))
  if (N < window) {
    warning("Length of data is less then window length")
    return(NULL)
  }
  
  blinks <- rep(F,N)
  if (!is.null(pupil)) {
    blinks <- do.call(blinkFUN, c(list(pupil, samplerate), list(...)))
    x[blinks] <- NA
    y[blinks] <- NA
    x <- na.approx(x)
    y <- na.approx(y)
  }
  
  ts <- 1 / samplerate
  
  sx <- sgolayfilt(x, n = window, p = order, m = 0, ts = ts)
  sy <- sgolayfilt(y, n = window, p = order, m = 0, ts = ts)
  
  cx <- rx / 2
  cy <- ry /2
  
  xa <- na.approx(subtended_angle(x, cy, cx, cy, rx, ry, sw, sh, ez, ex, ey))
  ya <- na.approx(subtended_angle(cx, y, cx, cy, rx, ry, sw, sh, ez, ex, ey))
      
  vx <- sgolayfilt(xa, n = window, p = order, m = 1, ts = ts)
  vy <- sgolayfilt(ya, n = window, p = order, m = 1, ts = ts)
  v <- sqrt(vx**2 + vy**2)
  
  ax <- sgolayfilt(xa, n = window, p = order, m = 2, ts = ts)
  ay <- sgolayfilt(ya, n = window, p = order, m = 2, ts = ts)
  a <- sqrt(ax**2 + ay**2)
  
  new("pva", time = 0:(N-1), ez = ez, ex = ex, ey = ey, x = x, y = y, sx = sx, xa = xa, 
      sy = sy, ya = ya,  v = v, a = a, sgolayfilt = c(order, window), rx = rx, ry = ry,
      samplerate = samplerate, blinks = blinks)
}