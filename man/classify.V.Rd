\name{classify.V}
\alias{classify.V}

\title{
  Gaze Data Classification (velocity)
}

\description{
  Classifies gaze data into saccades and fixations using a velocity threshold.
}

\usage{
  classify.VI(v, vt = 75)
}

\arguments{
  \item{v}{
    A vector of velocity samples
  }
  \item{vt}{
    The velocity threshold in degrees of visual angle per second
  }
}

\value{
  A classify object
}

\author{
  Ryan M. Hope
}

\examples{
  # Need an example
}