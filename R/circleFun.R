#' Circle Function
#'
#' Create a circle from comprised of limited set of points.
#' 
#' @param center the center point of the circle
#' @param diameter the diameter of the circle
#' @param npoints the number of points to use in the circle
#' @param round if TRUE, points will be rounded to nearest integer
#'
#' @return a data frame with columns for the x and y component of each point in the circle
#'
#' @export
#' 
#' @examples
#' c <- circleFun(center = c(5, 5), diameter = 2)
#' plot(c, asp=1)
#' 
circleFun <- function(center = c(0,0), diameter = 1, npoints = 50, round = FALSE) {
  r <- diameter / 2
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  if (round) {
    xx <- round(xx)
    yy <- round(yy)
  }
  return(data.frame(x = xx, y = yy))
}