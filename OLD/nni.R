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
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' d.f <- getFixations(d.c, d.pva)
#' d.nni <- nni(d.f[,c("x","y")])
#' str(d.nni)
#' 
nni <- function(x) {
  mean(nndist.default(x))/(.5*sqrt(chull_area(x)/nrow(x)))
}

#' @importFrom proto proto
#' @importFrom spatstat nnwhich.default
#' @import ggplot2
StatKnn <- proto(ggplot2:::Stat, {
  
  objname <- "knn"
  
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomSegment
  
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales,...)
  }
  
  calculate <- function(., data, scales, k=1, ...){
    data <- cbind(data[,c("x","y")], data[nnwhich.default(data, k=k),c("x","y")])
    colnames(data) <- c("x","y","xend","yend")
    data
  }
  
})

#' K-nearest neighbor statistic for ggplot2
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
stat_knn <- function(mapping=NULL, data=NULL, geom="segment", position="identity", k=1, ...) {
  StatKnn$new(mapping=mapping, data=data, geom=geom, position=position, k=k, ...)
}

#' @importFrom proto proto
#' @import ggplot2
GeomKnn <- proto(ggplot2:::GeomSegment, {
  objname <- "boundingbox"
  
  default_stat <- function(.) StatKnn  
})

#' Bounding Box Geom for ggplot2
#' 
#' @section Aesthetics: 
# \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "boundingbox")}
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
geom_knn <- function(mapping = NULL, data = NULL, stat = "knn", position = "identity", show_guide = FALSE, ...) {
  GeomKnn$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}