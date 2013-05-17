#' Extent (range) of fixations
#'
#' Calculates the distance between fixations in the horizontal and vertical meridians
#'   
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#'
#' @import ggplot2
#' @export
extent <- function(x) {
  c(max(x[,1])-min(x[,1]),max(x[,2])-min(x[,2]))
}

#' Bounding box of fixations
#'
#' Returns the coordinates of the bounding box around fixations
#'   
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#'
#' @import ggplot2
#' @export
bbox <- function(x) {
  data.frame(x=c(min(x[,1]),min(x[,1]),max(x[,1]),max(x[,1]),min(x[,1])),
             y=c(min(x[,2]),max(x[,2]),max(x[,2]),min(x[,2]),min(x[,2])))
}

#' Bounding Box Geom for ggplot2
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' @param ... extra arguments passed on to geom_path
#'
#' @import ggplot2
#' @export
geom_bbox <- function(x, ...) {
  geom_path(aes(x,y), bbox(x), ...)
}