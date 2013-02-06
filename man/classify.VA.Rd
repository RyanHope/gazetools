\name{classify.VA}
\alias{classify.VA}

\title{
  Gaze Data Classification (velocity-acceleration)
}

\description{
  Classifies gaze data into saccades and fixations using both velocity and acceleration thresholds.
}

\usage{
  classify.VA(v, a, vt=30, at=8000)
}

\arguments{
  \item{v}{
    A vector of velocity samples
  }
  \item{y}{
    A vector of acceleration samples
  }
  \item{vt}{
    The velocity threshold in degrees of visual angle per second
  }
  \item{at}{
    The acceleration threshold in degrees of visual angle per second squared
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