\name{extent}
\alias{extent}
\title{Extent (range) of fixations}
\usage{
  extent(x)
}
\arguments{
  \item{x}{coordinate vectors of points. This can be
  specified as a 2-column matrix x, a list x with two
  components}
}
\value{
  a list with two values, the horizontal (x) and vertical
  (y) range of points
}
\description{
  Calculates the distance between fixations in the
  horizontal and vertical meridians
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 500, 1680, 1050, 473.76, 296.1, smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
d.extent <- extent(d.f[,c("x","y")])
str(d.extent)
}

