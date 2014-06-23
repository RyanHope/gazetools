#' Compute Velocity and Acceleration Profiles for Raw Gaze Data
#'
#' Takes standard eye tracker gaze information and computes smoothed position, velocity
#' and acceleration profiles using a Savitzky-Golay filter.
#' 
#' @template 1p
#' @param samplerate the samplerate of the eyetracker
#' @template eye
#' @template sg
#' @param vt max velocity threshold
#' @param at max acceleration threshold
#' @param pupil a vector of class \code{numeric}; the vertical diameter of the pupil or a pupil status signal
#' @param blinkFUN the name of the blink detection algorithm to use
#' @param approxFUN the name of the interpolation function to use
#' @param ... arguments to be passed to blinkFUN
#'
#' @return an object of class \code{\link[=pva-class]{pva}}
#'
#' @importFrom zoo na.approx na.spline
#' @importFrom signal sgolay filter
#' 
#' @rdname pva
#' 
#' @export
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' str(d.pva)
#'  
pva <- function(x, y, samplerate, rx, ry, sw, sh, ez,
                ex = 0, ey = 0, order = 2, window = 11,
                vt=1000, at=100000, pupil = NULL, 
                blinkFUN = "detect_blinks.SW", 
                approxFUN = "na.spline", ...)
{
  x <- do.call(approxFUN, list(x, na.rm=FALSE))
  y <- do.call(approxFUN, list(y, na.rm=FALSE))
  if (length(ez)>1)
    ez <- do.call(approxFUN, list(ez, na.rm=FALSE))
  if (length(ex)>1)
    ex <- do.call(approxFUN, list(ex, na.rm=FALSE))
  if (length(ey)>1)
    ey <- do.call(approxFUN, list(ez, na.rm=FALSE))
  
  ts <- 1 / samplerate
  
  filter.smooth <- sgolay(p =  order, n = window, m = 0, ts = ts)
  sx <- filter(filter.smooth, x)
  sy <- filter(filter.smooth, y)
  
  cx <- rx / 2
  cy <- ry /2
  
  xa <- do.call(approxFUN, list(subtended_angle(x, cy, cx, cy, rx, ry, sw, sh, ez, ex, ey), na.rm=FALSE))
  ya <- do.call(approxFUN, list(subtended_angle(cx, y, cx, cy, rx, ry, sw, sh, ez, ex, ey), na.rm=FALSE))
  
  filter.velocity <- sgolay(p =  order, n = window, m = 1, ts = ts)
  vx <- filter(filter.velocity, xa)
  vy <- filter(filter.velocity, ya)
  v <- sqrt(vx**2 + vy**2)
  
  filter.acceleration <- sgolay(p =  order, n = window, m = 2, ts = ts)
  ax <- filter(filter.acceleration, xa)
  ay <- filter(filter.acceleration, ya)
  a <- sqrt(ax**2 + ay**2)
  
  N <- length(v)
  
  blinks <- rep(FALSE, N)
  
  if (is.null(pupil))
    pupil <- !((v>vt) + (a>at))
  else
    pupil[which(((v>vt)+(a>at))==TRUE)] <- 0
  
  blinks <- do.call(blinkFUN, c(list(na.approx(pupil, na.rm=FALSE), samplerate), list(...)))
  x[blinks] <- NA
  y[blinks] <- NA
  x <- do.call(approxFUN, list(x, na.rm=FALSE))
  y <- do.call(approxFUN, list(y, na.rm=FALSE))
  
  new("pva", time = 0:(length(v)-1) * ts, ez = ez, ex = ex, ey = ey, x = x, y = y, sx = sx, xa = xa,
      sy = sy, ya = ya,  v = v, a = a, sgolayfilt = c(order, window), rx = rx, ry = ry, sw = sw, sh = sh,
      samplerate = samplerate, blinks = blinks)
}