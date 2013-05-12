\docType{methods}
\name{pva.as.data.frame}
\alias{as.data.frame,pva,missing,missing-method}
\alias{mouldThreshold}
\alias{mouldThreshold,pva,character-method}
\alias{plot,pva,missing-method}
\alias{plot,voronoi_skewness,missing-method}
\alias{plot.pva}
\alias{plot.voronoi_skewness}
\alias{pva.as.data.frame}
\title{Plot pva}
\arguments{
  \item{x}{an object of class \code{"pva"}}

  \item{y}{an object of class \code{"classify"} (optional)}

  \item{dpva}{an object of class \code{"pva"}}

  \item{type}{'velocity' or 'acceleration'}

  \item{x}{an object of class \code{"voronoi_skewness"}}
}
\description{
  Plot the pva class

  Uses the Mould algorithm to determine the optimal
  velocity or acceleration threshold for classifying raw
  gaze data.

  Plot the voronoi_skewness class
}

