\name{getScanpath}
\alias{getScanpath}
\title{Get Scanpath}
\usage{
  getScanpath(x, y = NULL, rois, nearest = FALSE)
}
\arguments{
  \item{x,y}{the x and y coordinates of a set of points.
  Alternatively, a single argument x can be provided}

  \item{rois}{an object of class
  \code{\linkS4class{Polygons}}}

  \item{nearest}{if TRUE fixations not on an ROI will be
  moved to the nearest ROI}
}
\value{
  a vector of ROI ids
}
\description{
  Determines the scanpath given a set of fixations and a
  list of polygon ROIs
}

