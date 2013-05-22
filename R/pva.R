#' Class "pva"
#'
#' A class to hold position, velocity and acceleration of raw gaze data
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{ez}:}{vector of class \code{"numeric"}, containing the perpendicular distance from the viewer to the screen (mm)}
#'    \item{\code{ex}:}{vector of class \code{"numeric"}, containing the horizontal offset of the viewer from screen center (mm)}
#'    \item{\code{ey}:}{vector of class \code{"numeric"}, containing the vertical offset of the viewer from screen center (mm)}
#'    \item{\code{x}:}{vector of class \code{"numeric"}, containing the x coordinate of a point on a screen (pixels)}
#'    \item{\code{y}:}{vector of class \code{"numeric"}, containing the y coordinate of a point on a screen (pixels)}
#'    \item{\code{sx}:}{vector of class \code{"numeric"}, containing the smoothed x coordinate of a point on a screen (pixels)}
#'    \item{\code{sy}:}{vector of class \code{"numeric"}, containing the smoothed y coordinate of a point on a screen (pixels)}
#'    \item{\code{xa}:}{vector of class \code{"numeric"}, containing the x coordinate of a point in visual angle relative to the center of screen (degrees)}
#'    \item{\code{ya}:}{vector of class \code{"numeric"}, containing the y coordinate of a point in visual angle relative to the center of screen (degrees)}
#'    \item{\code{v}:}{vector of class \code{"numeric"}, containing the instantaneous velocity (degrees/s)}
#'    \item{\code{a}:}{vector of class \code{"numeric"}, containing the instantaneous acceleration (degrees/s^2)}
#'    \item{\code{rx}:}{the x resolution of the monitor (pixels)}
#'    \item{\code{ry}:}{the y resolution of the monitor (pixels)}
#'    \item{\code{samplerate}:}{the samplerate of the eyetracker}
#'    \item{\code{sgolayfilt}:}{parameters for the avitzky-Golay smoothing filter}
#'  }
#'
#' @docType class
#' @name pva-class
#' @rdname pva-class
#' @exportClass pva
setClass("pva", 
         representation(ez = "numeric",
                        ex = "numeric", ey = "numeric",
                        x = "numeric", y = "numeric",
                        sx = "numeric", sy = "numeric",
                        xa = "numeric", ya = "numeric",
                        v = "numeric", a = "numeric",
                        rx = "numeric", ry = "numeric",
                        samplerate = "numeric",
                        sgolayfilt = "numeric"))

#' Position, Velocity, Acceleration
#'
#' Takes standard eye tracker gaze information and computes smoothed position, velocity
#' and acceleration profiles using a Savitzky-Golay filter.
#' 
#' @template 1p
#' @param time the time corresponding to raw gaze samples
#' @param samplerate the samplerate of the eyetracker
#' @template eye
#' @template sg
#'
#' @return an object of class \code{\linkS4class{pva}}
#'
#' @import signal
#' @import zoo
#' 
#' @rdname pva
#' 
#' @export
#' 
#' @example example/pva.R
#'  
pva <- function(x, y, samplerate, rx, ry, sw, sh, ez,
                    ex = 0, ey = 0, order = 2, window = 11)
{
  N <- nrow(data.frame(x=x,y=y))
  if (N < window) {
    warning("Length of data is less then window length")
    return(NULL)
  }
  
  ts <- 1 / samplerate
  
  sx <- sgolayfilt(x, n = window, p = order, m = 0, ts = ts)
  sy <- sgolayfilt(y, n = window, p = order, m = 0, ts = ts)
  
  cx <- rx / 2
  cy <- ry /2
  
  xa <- na.approx(subtendedAngle(x, cy, cx, cy, rx, ry, sw, sh, ez, ex, ey))
  ya <- na.approx(subtendedAngle(cx, y, cx, cy, rx, ry, sw, sh, ez, ex, ey))
      
  vx <- sgolayfilt(xa, n = window, p = order, m = 1, ts = ts)
  vy <- sgolayfilt(ya, n = window, p = order, m = 1, ts = ts)
  v <- sqrt(vx**2 + vy**2)
  
  ax <- sgolayfilt(xa, n = window, p = order, m = 2, ts = ts)
  ay <- sgolayfilt(ya, n = window, p = order, m = 2, ts = ts)
  a <- sqrt(ax**2 + ay**2)
  
  new("pva", time = 0:(N-1), ez = ez, ex = ex, ey = ey, x = x, y = y, sx = sx, xa = xa, 
      sy = sy, ya = ya,  v = v, a = a, sgolayfilt = c(order, window), rx = rx, ry = ry,
      samplerate = samplerate)
}