require(spatstat)

#' Nearest Neighbor Index
#' 
#' Calculates the dispersion of a set of fixations.
#' 
#' When the NNI is equal to 1 when the distribution is random. Values lower than 1 suggest 
#' grouping, whereas values higher than 1 suggest regularity (i.e. the point pattern is 
#' dispersed in a non-random way). 
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#'
#' @import spatstat
#' @export
nni <- function(x) {
  mean(nndist(x))/(.5*sqrt(chull_area(x)/nrow(x)))
}