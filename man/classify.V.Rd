\name{classify.V}
\alias{classify.V}
\title{Gaze Data Classification (velocity)}
\usage{
  classify.V(v, vt = 75)
}
\arguments{
  \item{v}{a vector of instantaneous velocity values}

  \item{vt}{a velocity threshold}
}
\value{
  an object of class \code{\linkS4class{classify}}
}
\description{
  Classifies gaze data into saccades and fixations using a
  velocity threshold.
}

