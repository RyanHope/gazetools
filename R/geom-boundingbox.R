#' @import proto
#' @import ggplot2
GeomBoundingbox <- proto(ggplot2:::GeomRect, {
  objname <- "boundingbox"
  
  default_stat <- function(.) StatBoundingbox
  default_aes <- function(.) aes(colour="black", fill=NA, size=0.5, linetype=1, alpha = NA)
  
})

#' Bounding Box Geom for ggplot2
#' 
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "boundingbox")}
#' 
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param stat The statistical transformation to use on the data for this
#'    layer. 
#' @param position The position adjustment to use for overlapping points
#'    on this layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. This can 
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#' 
#' @export
geom_boundingbox <- function(mapping = NULL, data = NULL, stat = "boundingbox", position = "identity", show_guide = FALSE, ...) {
  GeomBoundingbox$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}