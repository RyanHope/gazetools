\name{getFixations}
\alias{getFixations}
\alias{getFixations,classify,pva-method}
\title{Get Fixations}
\description{
  Extracts the coordinates of fixations and their durations
  from \code{\linkS4class{classify}} and
  \code{\linkS4class{pva}} objects
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
}

