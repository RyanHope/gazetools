#' Angle to Pixels
#'
#' Calculated the distance (in pixels) at given a visual angle and distance.
#' 
#' @param alpha the target visual angle 
#' @template eye
#' @template 1p
#' 
#' @return number of pixels
#'
#' @export
#' 
#' @example example/angle2pixels.R
#'
angle2pixels <- function(alpha, rx, ry, sw, sh, ez,
                         ex = 0, ey = 0, x = rx / 2, y = ry / 2)
{
  d <- distance2point(x, y, rx, ry, sw, sh, ez, ex, ey)
  d * rx / sw * tan(alpha * pi / 180)
}