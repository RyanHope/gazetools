#' Region of Interest (ROI)
#'
#' Creates a polygon based region of interest from a set of coordinates.
#' 
#' @param coords a data frame of xy coordinates
#' @param ID a text based identifier for the new ROI
#'
#' @return an object of class \code{\linkS4class{ROI}}
#'
#' @importFrom sp Polygon
#' 
#' @rdname ROI
#' @export
#' 
ROI <- function (coords, ID) {
  new("ROI", Polygon(coords), ID=ID, center=c(mean(coords[,1]),mean(coords[,2])))
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

#' Fortify ROIs
#'
#' Method to convert a ROIs object into a data frame useful for plotting.
#' 
#' @param rois a ROIs object
#'
#' @return a data frame
#' 
#' @export
#'  
fortify.ROIs <- function(rois) {
  df <- ldply(rois, function(rois) cbind(rois@ID, data.frame(rois@coords)))
  colnames(df) <- c("id","x","y")
  df
}