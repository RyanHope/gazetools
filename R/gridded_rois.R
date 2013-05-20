#' Class "gridded_rois"
#'
#' A class to hold gridded ROI information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{x}:}{vector of class \code{"numeric"}, containing the x intercepts for vertical grid lines}
#'    \item{\code{y}:}{vector of class \code{"numeric"}, containing the y intercepts for horizontal grid lines}
#'    \item{\code{deltax}:}{the width of each grid cell}
#'    \item{\code{deltay}:}{the height of each grid cell}
#'    \item{\code{ncol}:}{the number of columns}
#'    \item{\code{nrow}:}{the number of rows}
#'  }
#'
#' @docType class
#' @name gridded_rois-class
#' @rdname gridded_rois-class
#' @exportClass gridded_rois
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

#' Gridded ROIs Geom for ggplot2
#' 
#' @param rois an object of class \code{\linkS4class{gridded_rois}}
#' @param ... extra arguments passed on to geom_segment
#'
#' @import ggplot2
#' @export
geom_gridded_rois <- function(rois, ...) {
  d <- data.frame()
  minx <- min(rois@x)
  maxx <- max(rois@x)
  miny <- min(rois@y)
  maxy <- max(rois@y)
  for (x in rois@x)
    d <- rbind(d, data.frame(x=x,xend=x,y=miny,yend=maxy))
  for (y in rois@y)
    d <- rbind(d, data.frame(x=minx,xend=maxx,y=y,yend=y))
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend),data=d,...)
}

#' Gridded ROIs Geom for ggplot2 (x-axis)
#' 
#' Discrete ROI scale for x axis (letters)
#' 
#' @param rois an object of class \code{\linkS4class{gridded_rois}}
#'
#' @import ggplot2
#' @export
scale_x_gridded_rois <- function(rois) {
  scale_x_continuous(breaks=head(rois@x,-1)+rois@deltax/2,labels=letters[1:rois@ncol])
}

#' Gridded ROIs Geom for ggplot2 (y-axis)
#' 
#' Discrete ROI scale for y axis (numbers)
#' 
#' @param rois an object of class \code{\linkS4class{gridded_rois}}
#'
#' @import ggplot2
#' @export
scale_y_gridded_rois <- function(rois) {
  scale_y_continuous(breaks=head(rois@y,-1)+rois@deltay/2,labels=1:rois@nrow)
}