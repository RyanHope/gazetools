\name{classify.VI}
\alias{classify.VI}
\title{Gaze Data Classification (velocity-iterative)}
\usage{
classify.VI(v, vt = 100, sigma = 6, samplerate = 500, min.fix = 0.04,
  min.sac = 0.01, blinks = FALSE, a = 0.7, b = (1 - a))
}
\arguments{
  \item{v}{a vector of instantaneous velocity values}

  \item{vt}{a velocity threshold}

  \item{sigma}{number of standard deviations used for upper
  saftey margin for peak velocity threshold, saccade
  velocity threshold is half this value, this parameter
  needs to be tuned for each eyetracker/dataset
  combination}

  \item{samplerate}{the samplerate in Hz of the eyetracker}

  \item{min.fix}{minimum fixations duration (in seconds)}

  \item{min.sac}{minimum saccade duration (in seconds)}

  \item{blinks}{vector of class \code{"logical"}, TRUE if
  samples belong to a blink}

  \item{a}{weight of saccade onset threshold for
  determining saccade offset threshold}

  \item{b}{weight of noise threshold for determining
  saccade offset threshold}
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
Other classify: \code{\link{as}}, \code{\link{as}};
  \code{\link{classify.MM}}; \code{\link{classify.VA}};
  \code{\link{classify.V}}
}

