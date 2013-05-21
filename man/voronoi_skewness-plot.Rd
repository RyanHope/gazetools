\docType{methods}
\name{plot.voronoi_skewness}
\alias{plot,voronoi_skewness,missing-method}
\alias{plot.voronoi_skewness}
\title{Plot voronoi_skewness}
\arguments{
  \item{x}{an object of class
  \code{\linkS4class{voronoi_skewness}}}
}
\description{
  Plot the voronoi_skewness class
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
d.vs <- voronoi_skewness(d.f[,c("x","y")], c(315,1365,0,1050))
plot(d.vs)
}

