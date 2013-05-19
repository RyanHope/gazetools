\name{bcea}
\alias{bcea}
\title{Bivariate contour ellipse area (BCEA)}
\usage{
  bcea(x, k = 0.9)
}
\arguments{
  \item{x}{coordinate vectors of points. This can be
  specified as a 2-column matrix x, a list x with two
  components}

  \item{k}{a proportion of points in a data set}
}
\value{
  an area (pixels^2)
}
\description{
  Calculates the area of an ellipse that encompasses a
  given proportion of points in a data set.
}

