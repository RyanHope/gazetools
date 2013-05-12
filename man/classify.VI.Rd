\name{classify.VI}
\alias{classify.VI}
\title{Gaze Data Classification (velocity-iterative)}
\usage{
  classify.VI(v, vt = 100, sigma = 6)
}
\arguments{
  \item{v}{a vector of instantaneous velocity values}

  \item{vt}{a velocity threshold}

  \item{sigma}{the number of standard deviations between
  fixation and saccade velocities}
}
\description{
  Classifies gaze data into saccades and fixations using an
  iteratively determined velocity threshold.
}

