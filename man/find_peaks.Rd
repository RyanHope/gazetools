\name{find_peaks}
\alias{find_peaks}
\title{Find Peak Ranges Above Threshold}
\usage{
find_peaks(x, threshold)
}
\arguments{
  \item{x}{a vector of data}

  \item{threshold}{a threshold above which to look for
  peaks}
}
\description{
Given a vector of data, find peaks in ranges of data that
exceede a set threshold
}
\examples{
find_peaks(c(1,2,3,4,5,4,3,2,1,1,1,2,3,4,3,2,1),3)
}

