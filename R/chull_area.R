require(sp)

#' Convex Hull Area
#' 
#' Computes the area of minimal convex polygon that spans all fixations.
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' 
#' @return an area (pixels^2)
#'
#' @import sp
#' @export
chull_area <- function(x) {
  hpts <- chull(x)
  hpts <- c(hpts, hpts[1])
  Polygon(x[hpts, ])@area
}

#' Convex Hull Geom for ggplot2
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' @param ... extra arguments passed on to geom_path
#'
#' @import ggplot2
#' @export
geom_chull <- function(x, ...) {
  hpts <- chull(x)
  hpts <- c(hpts, hpts[1])
  x <- x[hpts, ]
  geom_path(aes(x,y), x, ...)
}