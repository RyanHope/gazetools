\name{unique_ids}
\alias{unique_ids}
\title{Unique IDs}
\usage{
unique_ids(class)
}
\arguments{
  \item{class}{a vector}
}
\description{
Assigns unique monotonically increasing numeric ids to runs of identical elements in a vector.
}
\examples{
unique_ids(c("a","a","b","a","a","a","c","c"))
}

