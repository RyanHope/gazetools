\encoding{latin1}
\name{classify.VA}
\alias{classify.VA}
\title{Gaze Data Classification (velocity-acceleration)}
\usage{
  classify.VA(v, a, vt = 30, at = 8000, blinks = NULL)
}
\arguments{
  \item{v}{a vector of instantaneous velocity values}

  \item{vt}{a velocity threshold}

  \item{a}{a vector of instantaneous acceleration values}

  \item{at}{an acceleration threshold}

  \item{blinks}{vector of class \code{"logical"}, TRUE if
  samples belong to a blink}
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
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.VA(d.pva@v, d.pva@a)
str(d.c)
}
\references{
  Nyström, M., & Holmqvist, K. (2010). An adaptive
  algorithm for fixation, saccade, and glissade detection
  in eyetracking data. Behavior Research Methods, 42(1),
  188-204.
}
\seealso{
  Other classify: \code{\link{classify.MM}},
  \code{\link{classify.V}}, \code{\link{classify.VI}}
}

