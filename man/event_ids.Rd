\name{event_ids}
\alias{event_ids}
\title{Event IDs}
\usage{
  event_ids(x, event)
}
\arguments{
  \item{x}{a vector of data}

  \item{event}{the event to id}
}
\description{
  Assigns unique IDs to runs of a target events in a vector
  of data
}
\examples{
event_ids(c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,
            TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,
            FALSE,TRUE,TRUE), TRUE)
}

