\docType{class}
\name{classify-class}
\alias{classify-class}
\title{Class "classify"}
\description{
A class to hold gaze data classifications
}
\section{Slots}{
  \describe{ \item{\code{.Data}}{vector of class
  \code{"character"}, containing the gaze sample
  classification} \item{\code{fixation_ids}}{vector of
  class \code{"numeric"}, containing unique fixation ids}
  \item{\code{saccade_ids}}{vector of class
  \code{"numeric"}, containing unique saccade ids}
  \item{\code{algorithm}}{the algorithm used to classify
  the gaze data} \item{\code{thresholds}}{the threshold
  settings for the classification algorithm} }
}

