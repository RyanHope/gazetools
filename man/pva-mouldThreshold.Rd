\docType{methods}
\name{mouldThreshold}
\alias{mouldThreshold}
\alias{mouldThreshold,pva,character-method}
\title{Mould Threshold}
\arguments{
  \item{dpva}{an object of class \code{"pva"}}

  \item{type}{'velocity' or 'acceleration'}
}
\value{
  an object of class \code{\linkS4class{mould}}
}
\description{
  Uses the Mould algorithm to determine the optimal
  velocity or acceleration threshold for classifying raw
  gaze data.
}

