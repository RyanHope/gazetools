% Built by roxygen3 0.1
\docType{methods}
\name{plot,pva,classify-method}
\alias{plot,pva,classify-method}
\title{Plot a 'pva' Object with Classified Samples}
\usage{
  \S4method{plot}{pva,classify}(x, y, ...)
}
\arguments{
  \item{x}{an object of class \code{\link[=pva-class]{pva}}}

  \item{y}{an object of class \code{\link[=classify-class]{classify}} (optional)}
}

\description{
Plot an object of class 'pva' with classification information from an object of class 'classify'
}
\examples{
data(smi)
# Classified position, velocity and acceleration plot
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.VA(d.pva@v, d.pva@a)
plot(d.pva, d.c)

# Classified position, velocity and acceleration plot
# with blinks removed
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl, pupil=smi_dyl))
d.c <- classify.VA(d.pva@v, d.pva@a, blinks=d.pva@blinks)
plot(d.pva, d.c)
}

\keyword{methods}
