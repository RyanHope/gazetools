utils::globalVariables(c("y"))

#' Convex Hull Area
#' 
#' Computes the area of minimal convex polygon that spans all fixations.
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' 
#' @return an area (pixels^2)
#'
#' @importFrom sp Polygon
#' @export
#' 
#' @example example/pva.R
#' @example example/classify.V.R
#' @example example/getFixations.R
#' @example example/chull_area.R
#' @example example/chull_area-out.R
#' 
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
#' @importFrom ggplot2 geom_path aes
#' @export
geom_chull <- function(x, ...) {
  hpts <- chull(x)
  hpts <- c(hpts, hpts[1])
  x <- x[hpts, ]
  geom_path(aes(x,y), x, ...)
}