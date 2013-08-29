\name{detect_blinks}
\alias{detect_blinks}
\title{Detect Blinks}
\usage{
  detect_blinks(pupil, samplerate, window = 51,
    threshold = 1000)
}
\arguments{
  \item{pupil}{a vector of class \code{numeric}; the
  vertical diameter of the pupil}

  \item{samplerate}{the samplerate of the eyetracker}

  \item{window}{window length of smoothing filter}

  \item{threshold}{pupil diameter velocity threhold}
}
\value{
  a vector of class \code{logical}; indicates samples that
  belong to a blink
}
\description{
  Detects blinks based on pupil diameter velocity
}
\examples{
data(smi)
d.b <- with(smi, detect_blinks(smi_dyl, 500))
str(d.b)
}

