#' Class "coverage"
#'
#' A class to hold gridded ROI coverage information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}:}{the proportion of fixated ROI cells}
#'    \item{\code{coverage}:}{matrix of containing fixated ROI cells}
#'  }
#'
#' @docType class
#' @name coverage-class
#' @rdname coverage-class
#' @exportClass coverage
setClass("coverage", 
         representation(coverage = "matrix"),
         contains = "numeric")

#' Coverage
#' 
#' Calculates the proportion of ROI cells covered by fixations
#' 
#' @param rois an object of class \code{gridded_rois}
#' @param fixations coordinate vectors of fixations. This can be specified as a 2-column matrix x, 
#' a list x with two components
#'
#' @export
coverage <- function(rois, fixations) {
  N <- rois@nrow*rois@ncol
  m <- matrix(rep(F,N),rois@nrow,rois@ncol)
  for (i in 1:nrow(fixations)) {
    col <- ceiling((fixations[i,1]-rois@x[1])/rois@deltax)
    row <- ceiling((fixations[i,2]-rois@y[1])/rois@deltay)
    m[row,col] <- T
  }
  new("coverage", sum(m)/N, coverage=m)
}

#' Coverage Geom for ggplot2
#' 
#' @param rois an object of class \code{\linkS4class{gridded_rois}}
#' @param cv an object of class \code{\linkS4class{coverage}}
#' @param ... extra arguments passed on to geom_rect
#'
#' @import ggplot2
#' @export
geom_coverage <- function(rois, cv, ...) {
  d <- data.frame()
  covered <- which(cv@coverage==T, arr.ind=T)
  for (i in 1:nrow(covered))
    d <- rbind(d, data.frame(xmin=rois@x[covered[i,"col"]],
                             xmax=rois@x[covered[i,"col"]+1],
                             ymin=rois@y[covered[i,"row"]],
                             ymax=rois@y[covered[i,"row"]+1]))
  geom_rect(aes(xmax=xmax,xmin=xmin,ymax=ymax,ymin=ymin,x=NULL,y=NULL),data=d,...)
}