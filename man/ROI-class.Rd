\docType{class}
\name{ROI-class}
\alias{ROI-class}
\title{Class "ROI"}
\description{
An extension of the class \code{"Polygon"} that additionally contains a text ID
}
\section{Slots}{
  \describe{ \item{\code{ID}}{an object of class
  \code{"character"}; a text based ID}
  \item{\code{center}}{an object of class \code{"numeric"};
  the center coordinates of the ROI} \item{\code{labpt}}{an
  object of class \code{"numeric"}; an x, y coordinate pair
  forming the label point of the polygon}
  \item{\code{area}}{an object of class \code{"numeric"};
  the planar area of the polygon, does not respect
  projection as objects of this class have no projection
  defined} \item{\code{hole}}{an object of class
  \code{"logical"}; does the polygon seem to be a hole}
  \item{\code{ringDir}}{an object of class
  \code{"integer"}; the ring direction of the ring
  (polygon) coordinates, holes are expected to be
  anti-clockwise} \item{\code{coords}}{an object of class
  \code{"matrix"}; coordinates of the polygon; first point
  should equal the last point} }
}

