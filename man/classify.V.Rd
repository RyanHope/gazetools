\name{classify.V}
\alias{classify.V}
\title{Gaze Data Classification (velocity)}
\usage{
  classify.V(v, vt = 75)
}
\arguments{
  \item{v}{a vector of instantaneous velocity values}

  \item{vt}{a velocity threshold}
}
\value{
  an object of class \code{\linkS4class{classify}}
}
\description{
  Classifies gaze data into saccades and fixations using a
  velocity threshold.
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
str(d.c)
}
\seealso{
  Other classify: \code{\link{classify.VA}},
  \code{\link{classify.VI}}
}

