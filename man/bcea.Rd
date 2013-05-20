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
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 500, 1680, 1050, 473.76, 296.1, smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
d.bcea <- bcea(d.f[,c("x","y")])
str(d.bcea)
}

