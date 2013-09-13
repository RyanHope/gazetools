% Built by roxygen3 0.1
\docType{data}
\name{smi}
\alias{smi}
\alias{smi-package}
\title{Gaze data from a SMI RED 500 eyetracker at 500 Hz.}
\format{
\describe{
  \item{smi_time}{sample time}
  \item{smi_type}{sample type}
  \item{smi_sxl}{horizontal gaze location of left eye}
  \item{smi_sxr}{horizontal gaze location of right eye}
  \item{smi_syl}{vertical gaze location of left eye}
  \item{smi_syr}{vertical gaze location of right eye}
  \item{smi_dxl}{width of left pupil}
  \item{smi_dxr}{width of right pupil}
  \item{smi_dyl}{height of left pupil}
  \item{smi_dyr}{height of right pupil}
  \item{smi_exl}{horizontal offset of the left eye relative to the center of the screen}
  \item{smi_exr}{horizontal offset of the right eye relative to the center of the screen}
  \item{smi_eyl}{vertical offset of the left eye relative to the center of the screen}
  \item{smi_eyr}{vertical offset of the right eye relative to the center of the screen}
  \item{smi_ezl}{distance of the left eye to the center of the screen}
  \item{smi_ezr}{distance of the right eye to the center of the screen}
}
}
\usage{
  data(smi)
}
\description{
Gaze data of a participant performing a visual search task.
}
\details{
\itemize{
 \item{Samplerate was 500hz.}
 \item{Screen resolution was 1680 x 1050 (px).}
 \item{Screen size was 473.76 x 296.1 (mm).}
}
}
\keyword{datasets}
