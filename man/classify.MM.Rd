\name{classify.MM}
\alias{classify.MM}
\title{Gaze Data Classification (mixture model)}
\usage{
classify.MM(v, blinks = NULL)
}
\arguments{
  \item{v}{a vector of instantaneous velocity values}

  \item{blinks}{vector of class \code{"logical"}, TRUE if
  samples belong to a blink}
}
\value{
an object of class \code{\link[=classify-class]{classify}}
}
\description{
Uses a mixture model to classify gaze data into saccades and fixations.
}
\examples{
# Classification ignorning blinks
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl,
                       500, 1680, 1050, 473.76, 296.1,
                       smi_ezl, smi_exl, smi_eyl))
d.c <- classify.MM(d.pva@v)
str(d.c)

# Classification accounting for blinks
d.pva <- with(smi, pva(smi_sxl, smi_syl,
                       500, 1680, 1050, 473.76, 296.1,
                       smi_ezl, smi_exl, smi_eyl, pupil=smi_dxl))
d.c <- classify.MM(d.pva@v, blinks=d.pva@blinks)
str(d.c)
}
\references{
Ingmar Visser, Maarten Speekenbrink (2010). depmixS4: An R Package for Hidden Markov Models. Journal of Statistical Software, 36(7), 1-21.
}
\seealso{
Other classify: \code{\link{as}}, \code{\link{as}};
  \code{\link{classify.VA}}; \code{\link{classify.VI}};
  \code{\link{classify.V}}
}

