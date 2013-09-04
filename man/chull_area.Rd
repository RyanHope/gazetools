\name{chull_area}
\alias{chull_area}
\title{Convex Hull Area}
\usage{
  chull_area(x)
}
\arguments{
  \item{x}{coordinate vectors of points. This can be
  specified as a 2-column matrix x, a list x with two
  components}
}
\value{
  an area (pixels^2)
}
\description{
  Computes the area of minimal convex polygon that spans
  all fixations.
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl,
                       500, 1680, 1050, 473.76, 296.1,
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
d.cha <- chull_area(d.f[,c("x","y")])
d.cha
}

