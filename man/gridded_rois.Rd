\name{gridded_rois}
\alias{gridded_rois}
\title{Gridded ROIs}
\usage{
gridded_rois(xmin, xmax, ymin, ymax, ncol, nrow)
}
\arguments{
  \item{xmin}{the lower limit of the x axis}

  \item{xmax}{the upper limit of the x axis}

  \item{ymin}{the lower limit of the y axis}

  \item{ymax}{the upper limit of the y axis}

  \item{ncol}{the number of columns in the grid}

  \item{nrow}{the number of rows in the grid}
}
\value{
an object of class \code{\linkS4class{ROIs}}
}
\description{
Creates a grid of ROIs over a given 2-D region
}
\examples{
rois <- gridded_rois(315,1365,0,1050,nrow=11,ncol=11)
str(rois)
}

