% Built by roxygen3 0.1
\docType{methods}
\name{plot,pva,missing-method}
\alias{plot,pva,missing-method}
\title{Plot a 'pva' Object}
\usage{
  \S4method{plot}{pva,missing}(x, y, ...)
}
\arguments{
  \item{x}{an object of class \code{\link[=pva-class]{pva}}}

  \item{y}{the y coordinates of points in the plot, \emph{optional}
    if \code{x} is an appropriate structure.}
}

\description{
Plot an object of class 'pva'
}
\examples{
data(smi)
# Raw (unclassified) position, velocity and acceleration plot
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                      smi_ezl, smi_exl, smi_eyl))
plot(d.pva)
}

\keyword{methods}
