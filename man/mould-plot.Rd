% Built by roxygen3 0.1
\docType{methods}
\name{plot.mould}
\alias{plot,mould,missing-method}
\alias{plot.mould}
\title{Plot mould}
\usage{
  \S4method{plot}{mould,missing}(x, y, ...)
}
\arguments{
  \item{x}{an object of class \code{"mould"}}

  \item{y}{the y coordinates of points in the plot, \emph{optional}
    if \code{x} is an appropriate structure.}
}

\description{
Plot the mould class
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.mt <- mouldThreshold(d.pva@v, d.pva@samplerate)
plot(d.mt)
}

\keyword{methods}
