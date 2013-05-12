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
\description{
  Classifies gaze data into saccades and fixations using
  both velocity and acceleration thresholds.
}

