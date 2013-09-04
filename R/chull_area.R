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
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' d.f <- getFixations(d.c, d.pva)
#' d.cha <- chull_area(d.f[,c("x","y")])
#' d.cha
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