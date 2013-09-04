#' Class "ROI"
#'
#' An extension of the class \code{"Polygon"} that additionally contains a text ID
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{ID}}{an object of class \code{"character"}; a text based ID}
#'    \item{\code{labpt}}{an object of class \code{"numeric"}; an x, y coordinate pair forming the label point of the polygon}
#'    \item{\code{area}}{an object of class \code{"numeric"}; the planar area of the polygon, does not respect projection as objects of this class have no projection defined}
#'    \item{\code{hole}}{an object of class \code{"logical"}; does the polygon seem to be a hole}
#'    \item{\code{ringDir}}{an object of class \code{"integer"}; the ring direction of the ring (polygon) coordinates, holes are expected to be anti-clockwise}
#'    \item{\code{coords}}{an object of class \code{"matrix"}; coordinates of the polygon; first point should equal the last point}
#'  }
#'
#' @docType class
#' @name ROI-class
#' @rdname ROI-class
#' @exportClass ROI
#' @importFrom methods setClass
setClass("ROI", representation(ID="character"), contains="Polygon")

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
#' 
#' @export
#'  
ROI <- function (coords, ID) {
  new("ROI", Polygon(coords), ID=ID)
}

#' Class "ROIs"
#'
#' A list of ROI objects
#'
#' @docType class
#' @name ROIs-class
#' @rdname ROIs-class
#' @exportClass ROIs
#' @importFrom methods setClass
setClass("ROIs", contains="list", validity=function(object) {
  !any(sapply(object, function(x) !is(x, "ROI")))
})

#' Regions of Interest (ROIs)
#'
#' A list of regions of interest
#' 
#' @param rois a list of objects of class \code{"ROI"}
#'
#' @return an object of class \code{\linkS4class{ROIs}}
#'
#' @rdname ROIs
#' 
#' @export
#'  
ROIs <- function(rois) {
  stopifnot(is.list(rois))
  stopifnot(length(rois) > 0)
  names(rois) <- sapply(rois, function(x) x@ID)
  if (any(sapply(rois, function(x) !is(x, "ROI")))) 
    stop("rois not a list of ROI objects")
  new("ROIs", rois)
}