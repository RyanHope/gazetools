\name{pva.as.data.frame}
\alias{as.data.frame,pva,missing,missing-method}
\alias{pva.as.data.frame}
\title{Coerce object of class \code{\linkS4class{pva}} to a Data Frame}
\description{
  Coerce object of class \code{\linkS4class{pva}} to a Data
  Frame
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
head(as.data.frame(d.pva))
}

