\docType{methods}
\name{plot.mould}
\alias{plot,mould,missing-method}
\alias{plot.mould}
\title{Plot mould}
\arguments{
  \item{x}{an object of class \code{"mould"}}
}
\description{
  Plot the mould class
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 500, 1680, 1050, 473.76, 296.1, smi_ezl, smi_exl, smi_eyl))
d.mtv <- mouldThreshold(d.pva,"velocity")
d.mta <- mouldThreshold(d.pva,"acceleration")
plot(d.mtv)
}

