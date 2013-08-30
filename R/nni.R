#' Nearest Neighbor Index
#' 
#' Calculates the dispersion of a set of points.
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' 
#' @return The dispersion of a set of points; this value is equal to 1 when the distribution 
#' is random. Values lower than 1 suggest grouping, whereas values higher than 1 suggest 
#' regularity (i.e. the point pattern is dispersed in a non-random way). 
#'
#' @importFrom spatstat nndist.default
#' @export
#' 
#' @example example/pva.R
#' @example example/classify.V.R
#' @example example/getFixations.R
#' @example example/nni.R
#' @example example/nni-out.R
#' 
nni <- function(x) {
  mean(nndist.default(x))/(.5*sqrt(chull_area(x)/nrow(x)))
}