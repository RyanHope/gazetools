\name{voronoi_polygons}
\alias{voronoi_polygons}
\title{Voronoi Polygons}
\usage{
  voronoi_polygons(x, rw)
}
\arguments{
  \item{x}{a matrix of x,y cooridinates}

  \item{rw}{the coordinates of the corners of the
  rectangular window enclosing the voronoi cells, in the
  order (xmin, xmax, ymin, ymax).}
}
\value{
  an object of class
  \code{\linkS4class{SpatialPolygonsDataFrame}}
}
\description{
  Computes Voronoi polygons based on a set of x,y
  coordinates
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 500, 1680, 1050, 473.76, 296.1, smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
d.vp <- voronoi_polygons(d.f[,c("x","y")], c(315,1365,0,1050))
str(d.vp)
}

