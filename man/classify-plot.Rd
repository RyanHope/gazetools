\docType{methods}
\name{plot.classify}
\alias{plot,classify,pva-method}
\alias{plot.classify}
\title{Plot classify}
\arguments{
  \item{x}{an object of class
  \code{\linkS4class{classify}}}

  \item{y}{an object of class \code{\linkS4class{pva}}}
}
\description{
  Plot the classify class
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.VA(d.pva@v, d.pva@a)
plot(d.c, d.pva)
}

