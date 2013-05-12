\name{voronoi_skewness}
\alias{voronoi_skewness}
\title{Voronoi Skewness}
\usage{
  voronoi_skewness(d, rw)
}
\arguments{
  \item{d}{a matrix of x,y coordinates}

  \item{rw}{the coordinates of the corners of the
  rectangular window enclosing the voronoi cells, in the
  order (xmin, xmax, ymin, ymax).}
}
\description{
  Given a matrix of x,y coordinates computes the skewness
  of the area of the voronoi cells
}

