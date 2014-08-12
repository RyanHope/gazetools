#' Region of Interest (ROI)
#'
#' Creates a polygon based region of interest from a set of coordinates.
#' 
#' @param coords a data frame of xy coordinates
#' @param ID a text based identifier for the new ROI
#' @param layer the layer to which the ROI belongs
#' @param parent the id of the parent ROI
#'
#' @return an object of class \code{\linkS4class{ROI}}
#'
#' @importFrom sp Polygon
#' 
#' @rdname ROI
#' @export
#' 
ROI <- function (coords, ID, layer=0, parent=NA_character_) {
  new("ROI", Polygon(coords), ID=ID, center=c(mean(coords[,1]),mean(coords[,2])), layer=layer, parent=parent)
}

#' Regions of Interest (ROIs)
#'
#' A list of regions of interest
#' 
#' @param rois a list of objects of class \code{"ROI"}
#'
#' @return an object of class \code{\linkS4class{ROIs}}
#'
#' @rdname ROIs
#' @export
#'  
ROIs <- function(rois) {
  stopifnot(is.list(rois))
  stopifnot(length(rois) > 0)
  names(rois) <- NULL
  new("ROIs", rois)
}

names.ROIs <- function(x) unlist(lapply(x@ROIs, function(x) x@ID))

#' Fortify ROIs
#'
#' Method to convert a ROIs object into a data frame useful for plotting.
#' 
#' @param rois a ROIs object
#'
#' @return a data frame
#' 
#' @importFrom plyr ldply
#' @importFrom ggplot2 fortify
#' 
#' @export
#'  
fortify.ROIs <- function(rois) {
  df <- ldply(rois, function(r) cbind(r@ID, data.frame(r@coords), r@layer, r@parent))
  colnames(df) <- c("id","x","y","layer","parent")
  df
}