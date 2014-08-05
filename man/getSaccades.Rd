\name{getSaccades}
\alias{getSaccades}
\title{Get Saccades}
\usage{
getSaccades(class, dpva)
}
\arguments{
  \item{class}{an object of class \code{classify}}

  \item{dpva}{an object of class \code{pva}}
}
\description{
Extracts the start point, end point, distance and duration of saccades from \code{classify}
and \code{pva} objects
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl,
                       500, 1680, 1050, 473.76, 296.1,
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getSaccades(d.c, d.pva)
d.f
}

