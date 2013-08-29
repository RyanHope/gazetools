\docType{methods}
\name{plot.pva}
\alias{plot,pva,classify-method}
\alias{plot,pva,missing-method}
\alias{plot.pva}
\title{Plot pva}
\arguments{
  \item{x}{an object of class \code{\linkS4class{pva}}}

  \item{y}{an object of class \code{\linkS4class{classify}}
  (optional)}
}
\description{
  Plot the pva class
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
plot(d.pva)
d.c <- classify.VA(d.pva@v, d.pva@a)
plot(d.pva, d.c)
}

