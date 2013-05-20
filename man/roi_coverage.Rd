\name{roi_coverage}
\alias{roi_coverage}
\title{ROI Coverage}
\usage{
  roi_coverage(rois, fixations)
}
\arguments{
  \item{rois}{an object of class \code{gridded_rois}}

  \item{fixations}{coordinate vectors of fixations. This
  can be specified as a 2-column matrix x, a list x with
  two components}
}
\description{
  Calculates the proportion of ROI cells covered by
  fixations
}
\examples{
rois <- gridded_rois(315,1365,0,1050,nrow=11,ncol=11)
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl, smi_time, 500, 1680, 1050, 473.76, 296.1, smi_ezl, smi_exl, smi_eyl))
d.c <- classify.V(d.pva@v)
d.f <- getFixations(d.c, d.pva)
d.coverage <- roi_coverage(rois, d.f)
str(d.coverage)
}

