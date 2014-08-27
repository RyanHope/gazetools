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

rois.plot <- function(x, y, reverse_y = FALSE, alpha=1)
{
  ggplot(x) + geom_polygon(aes(x=x,y=y,color=reorder(id,layer),fill=reorder(id,layer)),alpha=alpha) + labs(color="ROIs",fill="ROIs")
}

#' Dynamic ROIs
#' 
#' A helper function for generating a DynamicROIs object.
#' 
#' @param start a vector of times indicating the start of a ROIs' existance at a particular location
#' @param end a vector of times indicating the end of an ROIs' existance at a particular location
#' @param id a vector ROI identifiers
#' @param coords a vector of ROI coordinates (a vector of x,y points (also a vector) encoded as a JSON string) 
#' @param layer a vector of layer identifiers for which each ROI belongs
#' @param parent a vector of parent ROI ids which indicates nesting of ROIs
#' 
#' @importFrom plyr dlply
#' @importFrom jsonlite fromJSON
#' 
#' @export
#' 
DynamicROIs <- function(start, end, id, coords, layer=0, parent=NA_character_) {
  new("DynamicROIs", dlply(data.frame(start=start, end=end, id=id, coords=coords, layer=layer, parent=parent), .(id), function(d) {
    id <- as.character(head(d$id,1))
    layer <- as.numeric(head(d$layer,1))
    parent <- as.character(head(d$parent,1))
    coords <- list()
    for (i in 1:nrow(d)) {
      dd <- t(as.data.frame(jsonlite:::fromJSON(as.character(d[i,"coords"]))))
      colnames(dd) <- c("x","y")
      rownames(dd) <- NULL
      ll <- nrow(dd)
      if (!((dd[1,"x"] == dd[ll,"x"]) && (dd[1,"y"] == dd[ll,"y"]))) {
        warning("ROI coordinates do not form a closed polygon, closing automatically.")
        dd <- rbind(dd, dd[1,])
      }
      coords[[i]] <- dd
    }
    new("DynamicROI",coords,ID=id,times=data.frame(begin=d$start,end=d$end),layer=layer,parent=parent)
  }))
}