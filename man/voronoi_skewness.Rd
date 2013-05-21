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
\value{
  an object of class \code{\linkS4class{voronoi_skewness}}
}
\description{
  Given a matrix of x,y coordinates computes the skewness
  of the area of the voronoi cells
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
d.vs <- voronoi_skewness(d.f[,c("x","y")], c(315,1365,0,1050))
str(d.vs)
}

