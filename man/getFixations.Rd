\name{getFixations}
\alias{getFixations}
\title{Get Fixations}
\usage{
getFixations(class, dpva, drop = T)
}
\arguments{
  \item{class}{an object of class
  \code{\linkS4class{classify}}}

  \item{dpva}{an object of class \code{\linkS4class{pva}}}

  \item{drop}{a boolean, if T (default) drop fixations at
  (0,0)}
}
\description{
Extracts the coordinates of fixations and their durations
from \code{\linkS4class{classify}} and
\code{\linkS4class{pva}} objects
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl,
                       500, 1680, 1050, 473.76, 296.1,
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
d.f
}

