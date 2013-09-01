\docType{methods}
\name{mouldThreshold}
\alias{mouldThreshold}
\alias{mouldThreshold,pva-method}
\title{Mould Threshold}
\arguments{
  \item{dpva}{an object of class \code{"pva"}}

  \item{type}{'velocity' or 'acceleration'}
}
\value{
  an object of class \code{\linkS4class{mould}}
}
\description{
  Uses the Mould algorithm to determine the optimal
  velocity or acceleration threshold for classifying raw
  gaze data.
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.mtv <- mouldThreshold(d.pva,"velocity")
d.mta <- mouldThreshold(d.pva,"acceleration")
str(d.mtv)
str(d.mta)
}
\references{
  Mould, M. S., Foster, D. H., Amano, K., & Oakley, J. P.
  (2012). A simple nonparametric method for classifying eye
  fixations. Vision research, 57, 18–25.
}

