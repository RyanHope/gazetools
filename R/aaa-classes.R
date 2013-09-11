#' Class "pva"
#'
#' A class to hold position, velocity and acceleration of raw gaze data
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{time}:}{vector of class \code{"numeric"}, containing the time corresponding to raw gaze samples}
#'    \item{\code{ez}:}{vector of class \code{"numeric"}, containing the perpendicular distance from the viewer to the screen (mm)}
#'    \item{\code{ex}:}{vector of class \code{"numeric"}, containing the horizontal offset of the viewer from screen center (mm)}
#'    \item{\code{ey}:}{vector of class \code{"numeric"}, containing the vertical offset of the viewer from screen center (mm)}
#'    \item{\code{x}:}{vector of class \code{"numeric"}, containing the x coordinate of a point on a screen (pixels)}
#'    \item{\code{y}:}{vector of class \code{"numeric"}, containing the y coordinate of a point on a screen (pixels)}
#'    \item{\code{sx}:}{vector of class \code{"numeric"}, containing the smoothed x coordinate of a point on a screen (pixels)}
#'    \item{\code{sy}:}{vector of class \code{"numeric"}, containing the smoothed y coordinate of a point on a screen (pixels)}
#'    \item{\code{xa}:}{vector of class \code{"numeric"}, containing the x coordinate of a point in visual angle relative to the center of screen (degrees)}
#'    \item{\code{ya}:}{vector of class \code{"numeric"}, containing the y coordinate of a point in visual angle relative to the center of screen (degrees)}
#'    \item{\code{v}:}{vector of class \code{"numeric"}, containing the instantaneous velocity (degrees/s)}
#'    \item{\code{a}:}{vector of class \code{"numeric"}, containing the instantaneous acceleration (degrees/s^2)}
#'    \item{\code{blinks}:}{vector of class \code{"logical"}, TRUE if samples belong to a blink}
#'    \item{\code{rx}:}{the x resolution of the monitor (pixels)}
#'    \item{\code{ry}:}{the y resolution of the monitor (pixels)}
#'    \item{\code{samplerate}:}{the samplerate of the eyetracker}
#'    \item{\code{sgolayfilt}:}{parameters for the avitzky-Golay smoothing filter}
#'  }
#'
#' @importFrom methods setClass
#' 
#' @docType class
#' @name pva-class
#' @rdname pva-class
#' 
#' @export
#' 
setClass("pva", 
         representation(time = "numeric", ez = "numeric",
                        ex = "numeric", ey = "numeric",
                        x = "numeric", y = "numeric",
                        sx = "numeric", sy = "numeric",
                        xa = "numeric", ya = "numeric",
                        v = "numeric", a = "numeric",
                        rx = "numeric", ry = "numeric",
                        blinks = "logical",
                        samplerate = "numeric",
                        sgolayfilt = "numeric"))

#' Class "classify"
#'
#' A class to hold gaze data classifications
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}}{vector of class \code{"character"}, containing the gaze sample classification}
#'    \item{\code{fixation_ids}}{vector of class \code{"numeric"}, containing unique fixation ids}
#'    \item{\code{saccade_ids}}{vector of class \code{"numeric"}, containing unique saccade ids}
#'    \item{\code{algorithm}}{the algorithm used to classify the gaze data}
#'    \item{\code{thresholds}}{the threshold settings for the classification algorithm}
#'  }
#'
#' @importFrom methods setClass
#'
#' @docType class
#' @name classify-class
#' @rdname classify-class
#' 
#' @export
#' 
setClass("classify", 
         representation(fixation_ids = "numeric", saccade_ids = "numeric",
                        algorithm="character", thresholds="numeric"),
         contains="character")

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
setClass("ROIs", contains="list", validity=function(object) {
  !any(sapply(object, function(x) !is(x, "ROI")))
})
setOldClass("ROIs")

#' Class "voronoi_skewness"
#'
#' A class to hold Voronoi skewness information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}}{the skewness of the area of the voronoi cells}
#'    \item{\code{polygons}}{an object of class \code{\linkS4class{ROIs}}}
#'  }
#'
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importFrom methods setClass
#'
#' @docType class
#' @name voronoi_skewness-class
#' @rdname voronoi_skewness-class
#' @exportClass voronoi_skewness
setClass("voronoi_skewness", 
         representation(polygons = "ROIs"),
         contains = "numeric")
