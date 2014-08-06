#' Plot ROIs
#' 
#' Plots ROIs as filled polygons
#' 
#' @param x an object of class \code{ROIs}
#' 
#' @docType methods
#' @importFrom ggplot2 ggplot geom_polygon aes
#' @importFrom methods setMethod
#' @rdname ROIs-methods
#' @name plot.ROIs
#' @export
#' @aliases plot,ROIs,missing-method
#' 
setMethod("plot", signature(x = "ROIs", y = "missing"), function(x, y, ...) rois.plot(x, NULL, ...))

rois.plot <- function(x, y, reverse_y = FALSE)
{
  ggplot(x) + geom_polygon(aes(x=x,y=y,color=reorder(id,layer),fill=reorder(id,layer))) + labs(color="ROIs",fill="ROIs")
}

