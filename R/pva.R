utils::globalVariables(c("v","a"))

#' Position, Velocity, Acceleration
#'
#' Takes standard eye tracker gaze information and computes smoothed position, velocity
#' and acceleration profiles using a Savitzky-Golay filter.
#'
#' @param x the horizontal coordinate of a point on a screen (pixels)
#' @param y the vertical coordinate of a point on a screen (pixels)
#' @param samplerate the number of samples taken in one second of time
#' @param rx the horizontal resolution of the screen (pixels)
#' @param ry the vertical resolution of the screen (pixels)
#' @param sw the physical screen width (mm)
#' @param sh the physical screen height (mm)
#' @param ez the perpendicular distance from the viewer to the screen (mm)
#' @param ex the horizontal offset of the viewer from screen center (mm)
#' @param ey the vertical offset of the viewer from screen center (mm)
#' @param timestamp vector of class \code{"numeric"} containing the external time corresponding to raw gaze samples
#' @param order the Savitzky-Golay filter order
#' @param window the Savitzky-Golay filter window length (must be odd)
#' @param vt the max peak velocity allowed for good data
#' @param at the max peak acceleration allowed for good data
#' @param blinks a vector of class \code{logical} indicating blinks or otherwise bad data in the velocity vector
#' @param blinkpadding the amount of time before and after each indictated blink that should included as part of the blink (s)
#'
#' @importFrom zoo na.spline
#' @importFrom signal filter sgolay
#'
#' @examples
#' data(smi)
#' d <- with(smi, pva(smi_sxl,smi_syl, 500,
#'                    1680, 1050, 473.76, 296.1,
#'                    smi_ezl, smi_exl, smi_eyl,
#'                    blinks=(smi_dyl==0|smi_dyr==0)))
#'
#' d <- with(highspeed, pva(x,y,1250,1024,768,.38,.30,.67,
#'                          blinks=(x==0|y==0)))
#'
#' @export
pva <- function(x, y, samplerate, rx, ry, sw, sh, ez,
                ex = 0, ey = 0, timestamp = -1,
                order = 2, window = 19,
                vt=1000, at=100000, blinks = NULL, blinkpadding = .06) {
  ts <- 1 / samplerate
  filter.velocity <- sgolay(p =  order, n = window, m = 1, ts = ts)
  filter.acceleration <- sgolay(p =  order, n = window, m = 2, ts = ts)

  N <- length(x)
  cx <- rep(rx / 2, N)
  cy <- rep(ry /2, N)

  data <- data.table(time = 0:(N-1) * ts, timestamp = timestamp,
                     x = x, y = y, ez = ez, ex = ex, ey = ey)

  if (!is.null(blinks)) {
    if (blinkpadding>0)
      data[,blinks:=pad_blinks(blinks, ceiling(blinkpadding/ts))]
    data[blinks==TRUE,`:=`(x=NA,y=NA)]
  } else {
    data[,blinks:=FALSE]
  }
  data[,`:=`(x=na.spline(x, na.rm=FALSE),
             y=na.spline(y, na.rm=FALSE))]

  xa <- data[,na.spline(subtended_angle(x, cy, cx, cy, rx, ry, sw, sh, ez, ex, ey), na.rm=FALSE)]
  ya <- data[,na.spline(subtended_angle(cx, y, cx, cy, rx, ry, sw, sh, ez, ex, ey), na.rm=FALSE)]

  vx <- filter(filter.velocity, xa)
  vy <- filter(filter.velocity, ya)

  ax <- filter(filter.acceleration, xa)
  ay <- filter(filter.acceleration, ya)

  data[,`:=`(v=sqrt(vx**2 + vy**2),
             a=sqrt(ax**2 + ay**2))]

  data[v>vt | a>at, `:=`(x=NA,y=NA,v=NA,a=NA)]

  filter.smooth <- sgolay(p =  order, n = window, m = 0, ts = ts)
  data[,`:=`(x=filter(filter.smooth, (na.spline(x, na.rm=FALSE))),
             y=filter(filter.smooth, (na.spline(y, na.rm=FALSE))),
             v=na.spline(v, na.rm=FALSE),
             a=na.spline(a, na.rm=FALSE))]
  setattr(data,"samplerate",samplerate)
  setattr(data,"window",window)
  setattr(data,"rx",rx)
  setattr(data,"ry",ry)
  setattr(data,"sw",sw)
  setattr(data,"sh",sh)
  setattr(data,"class",c("data.table","data.frame","pva"))
  data
}
