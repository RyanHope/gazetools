\name{classify.VA}
\alias{classify.VA}
\title{Gaze Data Classification (velocity-acceleration)}
\usage{
  classify.VA(v, a, vt = 30, at = 8000)
}
\arguments{
  \item{v}{a vector of instantaneous velocity values}

  \item{vt}{a velocity threshold}

  \item{a}{a vector of instantaneous acceleration values}

  \item{at}{an acceleration threshold}
}
\value{
  an object of class \code{\linkS4class{classify}}
}
\description{
  Classifies gaze data into saccades and fixations using
  both velocity and acceleration thresholds.
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.VA(d.pva@v, d.pva@a)
str(d.c)
}
\seealso{
  Other classify: \code{\link{classify.V}},
  \code{\link{classify.VI}}
}

