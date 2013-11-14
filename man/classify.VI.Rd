% Built by roxygen3 0.1
\name{classify.VI}
\alias{classify.VI}
\title{Gaze Data Classification (velocity-iterative)}
\usage{
  classify.VI(v, vt = 100, sigma = 6, blinks = NULL)
}
\arguments{
  \item{sigma}{the number of standard deviations between fixation and saccade velocities}

  \item{v}{a vector of instantaneous velocity values}

  \item{vt}{a velocity threshold}

  \item{blinks}{vector of class \code{"logical"}, TRUE if samples belong to a blink}
}

\value{
an object of class \code{\link[=classify-class]{classify}}
}
\description{
Classifies gaze data into saccades and fixations using an iteratively determined velocity threshold.
}
\examples{
# Classification ignorning blinks
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.VI(d.pva@v)
str(d.c)

# Classification accounting for blinks
d.pva <- with(smi, pva(smi_sxl, smi_syl, 
                       500, 1680, 1050, 473.76, 296.1, 
                       smi_ezl, smi_exl, smi_eyl, pupil=smi_dxl))
d.c <- classify.VI(d.pva@v, blinks=d.pva@blinks)
str(d.c)
}

\seealso{
Other classify: \code{\link{as}}, \code{\link{classify.MM}}, \code{\link{classify.V}}, \code{\link{classify.VA}}
}
