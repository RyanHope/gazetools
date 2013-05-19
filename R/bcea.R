#' Bivariate contour ellipse area (BCEA)
#' 
#' Calculates the area of an ellipse that encompasses a given proportion 
#' of points in a data set.
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' @param k a proportion of points in a data set
#' 
#' @return an area (pixels^2)
#'
#' @export
bcea <- function(x, k=.90) {
  2 * k * pi * sd(x[,1]) * sd(x[,2]) * sqrt((1 - cor(x[,1],x[,2])^2))
}