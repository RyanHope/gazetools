

#' Class "mould"
#'
#' A class to hold mould threshold information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}}{a threshold value}
#'    \item{\code{thresholds}}{vector of class \code{"numeric"}}
#'    \item{\code{resp1}}{vector of class \code{"numeric"}}
#'    \item{\code{resp2}}{vector of class \code{"numeric"}}
#'    \item{\code{gap}}{vector of class \code{"numeric"}}
#'  }
#'
#' @importFrom methods setClass
#' 
#' @docType class
#' @name mould-class
#' @rdname mould-class
#' 
#' @export
#' 
setClass("mould", 
         representation(thresholds = "numeric",
                        resp1 = "numeric",
                        resp2 = "numeric",
                        gap = "numeric"), 
         contains = "numeric")

#' Class "ROI"
#'
#' An extension of the class \code{"Polygon"} that additionally contains a text ID
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{ID}}{an object of class \code{"character"}; a text based ID}
#'    \item{\code{center}}{an object of class \code{"numeric"}; the center coordinates of the ROI}
#'    \item{\code{labpt}}{an object of class \code{"numeric"}; an x, y coordinate pair forming the label point of the polygon}
#'    \item{\code{area}}{an object of class \code{"numeric"}; the planar area of the polygon, does not respect projection as objects of this class have no projection defined}
#'    \item{\code{hole}}{an object of class \code{"logical"}; does the polygon seem to be a hole}
#'    \item{\code{ringDir}}{an object of class \code{"integer"}; the ring direction of the ring (polygon) coordinates, holes are expected to be anti-clockwise}
#'    \item{\code{coords}}{an object of class \code{"matrix"}; coordinates of the polygon; first point should equal the last point}
#'  }
#'
#' @importFrom methods setClass
#' @importClassesFrom sp Line Polygon
#' 
#' @docType class
#' @name ROI-class
#' @rdname ROI-class
#' 
#' @export
#' 
setClass("ROI", representation(ID="character",center="numeric"), contains="Polygon")

#' Class "ROIs"
#'
#' A list of ROI objects
#' 
#' @importFrom methods setClass setOldClass
#'
#' @docType class
#' @name ROIs-class
#' @rdname ROIs-class
#' 
#' @export
#' 
setClass("ROIs", representation(), contains="list", validity=function(object) {
  !any(sapply(object, function(x) !is(x, "ROI")))
})
#setOldClass("ROIs")

#' Class "VoronoiPolygons"
#'
#' A class to hold Voronoi tessellation information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}}{an object of class \code{\linkS4class{ROIs}}}
#'    \item{\code{skewness}}{the skewness of the area of the voronoi cells}
#'  }
#'
#' @importFrom methods setClass
#'
#' @docType class
#' @name VoronoiPolygons-class
#' @rdname VoronoiPolygons-class
#' 
#' @export
#' 
setClass("VoronoiPolygons", representation(skewness = "numeric"), contains = "ROIs")

#' Class "Scanpath"
#'
#' Holds ROIs and a vector of ROI IDs in the order they were fixated.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}}{a vector of class \code{"character"}; fixated ROI IDs}
#'    \item{\code{ROIs}}{an object of class \code{"ROIs"}; the set of ROIs}
#'  }
#'
#' @importFrom methods setClass
#' 
#' @docType class
#' @name Scanpath-class
#' @rdname Scanpath-class
#' 
#' @export
#' 
setClass("Scanpath", representation(ROIs="ROIs"), contains="character")

#' Class "Coverage"
#'
#' Holds a Scanpath and the proportion of ROIs fixated
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}}{an object of class \code{"numeric"}; the proportion of ROIs fixated}
#'    \item{\code{Scanpath}}{an object of class \code{"Scanpath"}; the scanpath}
#'  }
#'
#' @importFrom methods setClass
#' 
#' @docType class
#' @name Coverage-class
#' @rdname Coverage-class
#' 
#' @export
#' 
setClass("Coverage", representation(Scanpath="Scanpath"), contains="numeric")