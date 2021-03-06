% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gazetools-package.R
\docType{data}
\name{lc120}
\alias{lc120}
\title{Gaze data from a LC Technologies binocular eyetracker at 120 Hz.}
\format{\describe{
  \item{status}{signal status}
  \item{pupil}{pupil size}
  \item{x}{horizontal gaze location}
  \item{y}{vertical gaze location}
  \item{field}{field/camera number}
  \item{timestamp}{time stamp}
}}
\usage{
data(lc120)
}
\description{
Gaze data of a participant performing a visual search task.
}
\details{
\itemize{
 \item Samplerate was 120hz (alternating 60hz per eye).
 \item Screen resolution was 1280 x 1024 (px).
 \item Screen size was 33.97 x 27.31 (cm).
 \item Viewing distance was 58.74 (cm).
 \item Vertical viewing offset was 4.55 (cm).
}
}
\examples{
data(lc120)
lc120

}
\keyword{datasets}

