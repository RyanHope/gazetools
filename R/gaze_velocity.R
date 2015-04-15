utils::globalVariables(c("v","a"))

#' Gaze Velocity
#'
#' Computes instantaneous gaze velocity using a Savitzky-Golay filter.
#'
#' @param x the horizontal coordinate of a point on a screen (pixels)
#' @param y the vertical coordinate of a point on a screen (pixels)
#' @param ez the perpendicular distance from the viewer to the screen (mm)
#' 
#' @param ex the horizontal offset of the viewer from screen center (mm)
#' @param ey the vertical offset of the viewer from screen center (mm)
#' @param timestamp vector of class \code{"numeric"} containing the external time corresponding to raw gaze samples
#' @param blinks a vector of class \code{logical} indicating blinks or otherwise bad data in the velocity vector
#' 
#' @param samplerate the number of samples taken in one second of time
#' @param screen.resolution vector of class \code{"numeric"}, the resolution of the screen (pixels)
#' @param screen.dimensions vector of class \code{"numeric"}, the physical dimensions of the screen (mm)
#' 
#' @param minsac the minimum saccade duration in seconds
#' @param vt the max peak velocity allowed for good data
#' @param at the max peak acceleration allowed for good data
#'
#' @importFrom zoo na.spline
#' @importFrom signal filter sgolay
#'
#' @examples
#' data(smi)
#' d <- with(smi, gaze_velocity(smi_sxl, smi_syl,
#'                              smi_ezl, smi_exl, smi_eyl,
#'                              timestamp=smi_time, blinks=(smi_dyl==0),
#'                              500, 1680, 1050, 473.76, 296.1)
#' d
#'
#' d <- with(highspeed, gaze_velocity(x,y,1250,1024,768,.38,.30,.67,
#'                          blinks=(x==0|y==0)))
#' d
#'
#' @export
gaze_velocity <- function(x, y, ez, ex = 0, ey = 0,
                          timestamp = -1, blinks = NULL,
                          samplerate, screen.resolution, screen.dimensions,
                          minsac=.02, vt=1000, at=100000) {
  order <- 2
  ts <- 1 / samplerate
  window <- 2 * ceiling(minsac/ts) + 3
  
  filter.smooth <- sgolay(p =  order, n = window, m = 0, ts = ts)
  filter.velocity <- sgolay(p =  order, n = window, m = 1, ts = ts)
  filter.acceleration <- sgolay(p =  order, n = window, m = 2, ts = ts)
  
  .x <- x
  .y <- y
  
  N <- length(x)
  cx <- rep(0, N)
  cy <- rep(0, N)
  
  data <- data.table(time = 0:(N-1) * ts, timestamp = timestamp, x = x, y = y, ez = ez, ex = ex, ey = ey)
  
  if (!is.null(blinks)) {
    data[,blinks:=(abs(filter(filter.smooth,blinks))>0)]
    data[blinks==TRUE,`:=`(x=0,y=0)]
  } else {
    data[,blinks:=FALSE]
  }
  
  #   data[,`:=`(x=na.spline(x, na.rm=FALSE),
  #              y=na.spline(y, na.rm=FALSE))]
  
  xa <- data[,subtended_angle(x, cy, cx, cy, rx, ry, sw, sh, ez, ex, ey)]
  ya <- data[,subtended_angle(cx, y, cx, cy, rx, ry, sw, sh, ez, ex, ey)]
  
  vx <- filter(filter.velocity, xa)
  vy <- filter(filter.velocity, ya)
  
  ax <- filter(filter.acceleration, xa)
  ay <- filter(filter.acceleration, ya)
  
  data[,`:=`(vx=vx,vy=vy,
             v=sqrt(vx**2 + vy**2),
             a=sqrt(ax**2 + ay**2))]
  
  data[,noise:=FALSE]
  data[v>vt | a>at, noise:=TRUE]
  
  data[,`:=`(x=.x,
             y=.y,
             sx=filter(filter.smooth,x),
             sy=filter(filter.smooth,y)
  )]
  
  setattr(data,"samplerate",samplerate)
  setattr(data,"window",window)
  setattr(data,"minsac",minsac)
  setattr(data,"rx",rx)
  setattr(data,"ry",ry)
  setattr(data,"sw",sw)
  setattr(data,"sh",sh)
  setattr(data,"class",c("data.table","data.frame","pva"))
  data
}
