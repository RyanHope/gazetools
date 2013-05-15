require(reshape)
require(ggplot2)
require(signal)
require(zoo)

#' Class "pva"
#'
#' A class to hold position, velocity and acceleration of raw gaze data
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{time}:}{vector of class \code{"numeric"}, containing the time corresponding to raw gaze samples}
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
         representation(time = "numeric", ez = "numeric",
                        ex = "numeric", ey = "numeric",
                        x = "numeric", y = "numeric",
                        sx = "numeric", sy = "numeric",
                        xa = "numeric", ya = "numeric",
                        v = "numeric", a = "numeric",
                        rx = "numeric", ry = "numeric",
                        samplerate = "numeric",
                        sgolayfilt = "numeric"))

#' @rdname pva-methods
#' @aliases as.data.frame,pva,missing,missing-method
#' @name pva.as.data.frame
#' @export
setMethod("as.data.frame", signature(x = "pva", row.names = "missing", optional = "missing"),
          function(x) {
            data.frame(time = x@time, ez = x@ez, ex = x@ex, ey = x@ey, x = x@x, y = x@y,
                       sx = x@sx, sy = x@sy, xa = x@xa, ya = x@ya, v = x@v, a = x@a, 
                       rx = x@rx, ry = x@ry, samplerate = x@samplerate)
          }
)

#' Plot pva
#' 
#' Plot the pva class
#' 
#' @param x an object of class \code{"pva"}
#' @param y an object of class \code{"classify"} (optional)
#' 
#' @docType methods
#' @import reshape
#' @import ggplot2
#' @rdname pva-methods
#' @name plot.pva
#' @export
#' @aliases plot,pva,missing-method
setMethod("plot", signature(x = "pva", y = "missing"), function(x, y) pva.plot(x))

pva.plot <- function(x, y=NULL)
{
  d <- as.data.frame(x)
  if (!is.null(y) & class(y)=="classify") {
    d$class <- factor(y)
    d <- melt(d, id=c("time","class"))
  } else
    d <- melt(d, id=c("time"))
  d <- subset(d, variable=="sx" | variable=="sy" | variable=="v" | variable=="a")
  d$variable <- factor(d$variable,labels=c("Gaze X", "Gaze Y", "Velocity", "Acceleration"))
  if (!is.null(y) & class(y)=="classify") {
    thresholds <- data.frame(variable=levels(d$variable))
    if (length(y@thresholds) > 1) 
      thresholds$intercept <- c(NA,NA,y@thresholds)
    else
      thresholds$intercept <- c(NA,NA,y@thresholds,NA)
    p <- ggplot(d) + geom_point(aes(x=time, y=value, color=class)) + 
      geom_hline(data=thresholds,aes(yintercept=intercept)) +
      scale_color_manual(values=c("black", "red"))
  } else
    p <- ggplot(d) + geom_point(aes(x=time, y=value))
  p + facet_grid(variable~., scales="free_y") + ylab("") + xlab("Time (s)") +
    theme(legend.position = "top")
}

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
#' @import signal
#' @import zoo
#' @rdname pva
#' @export
pva <- function(x, y, time, samplerate, rx, ry, sw, sh, ez,
                    ex = 0, ey = 0, order = 2, window = 11)
{
  if (length(time) < window) return(NULL)
  
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
  
  new("pva", time = time, ez = ez, ex = ex, ey = ey, x = x, y = y, sx = sx, xa = xa, 
      sy = sy, ya = ya,  v = v, a = a, sgolayfilt = c(order, window), rx = rx, ry = ry,
      samplerate = samplerate)
}