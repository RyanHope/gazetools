\name{pvaPlot}
\alias{pvaPlot}

\title{
  Position, Velocity, Acceleration Plot
}

\description{
  Takes a pva data.frame and plots the x and y gaze position as well as the 
  instantaneous velocity and acceleration over time.
}
\usage{
  pvaPlot(d, layout = c(1, NA), thresholds = NULL)
}

\arguments{
  \item{d}{
    A pva data.frame returned by pva()
  }
  \item{layout}{
    A lattice compat layout.
  }
  \item{thresholds}{
    A list of thresholds. ~This needs improvement
  }
}

\author{
  Ryan M. Hope
}

\examples{
  # Need an example
}