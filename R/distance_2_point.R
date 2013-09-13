#' Distance to Point
#'
#' Takes an x and y screen coordinate and returns the physical distance
#' from the observer to that point on the screen.
#' 
#' @template 1p
#' @template eye
#'
#' @export
#' 
#' @examples
#' distance_2_point(840, 525, 1680, 1050, 473.76, 296.1, 750)
#' 
distance_2_point <- function(x, y, rx, ry, sw, sh, ez, ex = 0, ey = 0)
{
  dx <- x / rx * sw - sw / 2 + ex
  dy <- y / ry * sh - sh / 2 - ey
  sqrt(ez**2 + dx**2 + dy**2)
}