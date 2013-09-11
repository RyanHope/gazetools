#' @importFrom proto proto
#' @import ggplot2
StatBoundingbox <- proto(ggplot2:::Stat, {
  
  objname <- "boundingbox"
  
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomRect
  
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales,...)
  }
  
  calculate <- function(., data, scales, ...){
    data.frame(xmin=min(data$x), xmax=max(data$x), ymin=min(data$y), ymax=max(data$y))
  }
  
})

#' Bounding box statistic for ggplot2
#' 
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geometric object to use display the data 
#' @param position The position adjustment to use for overlappling points
#'    on this layer
#' @param ... other arguments passed on to \code{\link{layer}}. This can 
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#' 
#' @export
stat_boundingbox <- function(mapping=NULL, data=NULL, geom="path", position="identity", ...) {
  StatBoundingbox$new(mapping=mapping, data=data, geom=geom, position=position, ...)
}