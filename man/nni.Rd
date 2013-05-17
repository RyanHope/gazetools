\name{nni}
\alias{nni}
\title{Nearest Neighbor Index}
\usage{
  nni(x)
}
\arguments{
  \item{x}{coordinate vectors of points. This can be
  specified as a 2-column matrix x, a list x with two
  components}
}
\description{
  Calculates the dispersion of a set of fixations.
}
\details{
  When the NNI is equal to 1 when the distribution is
  random. Values lower than 1 suggest grouping, whereas
  values higher than 1 suggest regularity (i.e. the point
  pattern is dispersed in a non-random way).
}

