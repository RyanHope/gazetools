#' Class "gridded_rois"
#'
#' A class to hold gridded ROI information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{x}}{vector of class \code{"numeric"}, containing the x intercepts for vertical grid lines}
#'    \item{\code{y}}{vector of class \code{"numeric"}, containing the y intercepts for horizontal grid lines}
#'    \item{\code{deltax}}{the width of each grid cell}
#'    \item{\code{deltay}}{the height of each grid cell}
#'    \item{\code{ncol}}{the number of columns}
#'    \item{\code{nrow}}{the number of rows}
#'  }
#'
#' @docType class
#' @name gridded_rois-class
#' @rdname gridded_rois-class
#' @exportClass gridded_rois
#' @importFrom methods setClass
setClass("gridded_rois", 
         representation(x = "numeric", y = "numeric",
                        deltax = "numeric", deltay = "numeric",
                        nrow = "numeric", ncol = "numeric"))

#' Gridded ROIs
#'
#' Creates a grid of ROIs over a given 2-D region
#' 
#' @param xmin the lower limit of the x axis
#' @param xmax the upper limit of the x axis
#' @param ymin the lower limit of the y axis
#' @param ymax the upper limit of the y axis
#' @param ncol the number of columns in the grid
#' @param nrow the number of rows in the grid
#' 
#' @return an object of class \code{\linkS4class{gridded_rois}}
#'
#' @rdname gridded_rois
#' @export
#' 
#' @example example/gridded_rois.R
#' @example example/gridded_rois-out.R
#' 
gridded_rois <- function(xmin, xmax, ymin, ymax, ncol, nrow) {
  deltax <- (xmax-xmin)/ncol
  deltay <- (ymax-ymin)/nrow
  x <- seq(xmin,xmax,length.out=(ncol+1))
  y <- seq(ymin,ymax,length.out=(nrow+1))
  new("gridded_rois",x=x,y=y,deltax=deltax,deltay=deltay,ncol=ncol,nrow=nrow)
}

#' Grid Geom for ggplot2
#' 
#' @param xintercept a vector of x intercepts
#' @param yintercept a vector of y intercepts
#' @param ... extra arguments passed on to geom_segment
#'
#' @importFrom ggplot2 geom_hline geom_vline
#' @export
geom_grid <- function(xintercept, yintercept, ...) {
  list(geom_hline(yintercept=yintercept, ...), geom_vline(xintercept=xintercept, ...))
}

#' Grid Scales for ggplot2
#' 
#' Discrete grid scales
#' 
#' @param xintercept a vector of x intercepts
#' @param yintercept a vector of y intercepts
#' @param outer boolean, if TRUE xintercept and yintercept are assumed to contain intercepts for the plot limits
#'
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#' @export
scale_grid <- function(xintercept, yintercept, outer=T) {
  xoffset <- diff(xintercept)[1]/2
  yoffset <- diff(yintercept)[1]/2
  if (outer) {
    xbreaks <- head(xintercept,-1) + xoffset
    ybreaks <- head(yintercept,-1) + yoffset
    ncol <- length(xintercept) - 1
    nrow <- length(yintercept) - 1
  } else {
    xbreaks <- xintercept - xoffset
    ybreaks <- yintercept - xoffset
    ncol <- length(xintercept) + 1
    nrow <- length(yintercept) + 1
  }
  list(scale_x_continuous(breaks=xbreaks,labels=letters[1:ncol]),
       scale_y_continuous(breaks=ybreaks,labels=1:nrow))
}