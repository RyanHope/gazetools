#' @importFrom proto proto
#' @import ggplot2
StatEllipse <- proto(ggplot2:::Stat, {
  
  objname <- "ellipse"
  
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPath
  
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales,...)
  }
  
  calculate <- function(., data, scales, level = 0.75, segments = 51,...){
    bceFun(data, level, segments)   
  }
  
})

#' Ellipse statistic for ggplot2
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
stat_ellipse <- function(mapping=NULL, data=NULL, geom="path", position="identity", ...) {
  StatEllipse$new(mapping=mapping, data=data, geom=geom, position=position, ...)
}