\name{classify.VI}
\alias{classify.VI}

\title{
  Gaze Data Classification (velocity-iterative)
}

\description{
  Classifies gaze data into saccades and fixations using an iteratively determined velocity threshold.
}

\usage{
  classify.VI(v, sigma=6, vpt=100)
}

\arguments{
  \item{v}{
    A vector of velocity samples
  }
  \item{sigma}{
    The number of standard deviations used each iteration step to determine velocity threshold
  }
  \item{vpt}{
    The initial velocity threshold
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