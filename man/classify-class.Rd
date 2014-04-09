\docType{class}
\name{classify-class}
\alias{classify-class}
\title{Class "classify"}
\description{
A class to hold gaze data classifications
}
\section{Slots}{

\describe{
\item{\code{.Data}}{vector of class \code{"character"}, containing the gaze sample classification}

\item{\code{event_ids}}{vector of class \code{"numeric"}, containing unique event ids}

\item{\code{blink_ids}}{vector of class \code{"numeric"}, containing unique blink ids}

\item{\code{fixation_ids}}{vector of class \code{"numeric"}, containing unique fixation ids}

\item{\code{saccade_ids}}{vector of class \code{"numeric"}, containing unique saccade ids}

\item{\code{glissade_ids}}{vector of class \code{"numeric"}, containing unique glissade ids}

\item{\code{quality}}{vector of class \code{"numeric"}, containing the quality of each sample}

\item{\code{algorithm}}{the algorithm used to classify the gaze data}

\item{\code{thresholds}}{the threshold settings for the classification algorithm}
}}

