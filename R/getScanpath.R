#' Scanpath
#'
#' Determines the scanpath given a set of fixations and a list of polygon ROIs
#' 
#' @param x,y the x and y coordinates of a set of points. Alternatively, a single argument x can be provided
#' @param rois an object of class \code{\linkS4class{Polygons}}
#' @param nearest if TRUE fixations not on an ROI will be moved to the nearest ROI
#'
#' @importFrom sp point.in.polygon
#'
#' @return an object of class \code{\linkS4class{Scanpath}}
#'
#' @export
#' 
scanpath <- function(x, y = NULL, rois, nearest = FALSE) {
  if (!is(rois, "ROIs"))
    stop("rois is not of class ROIs")
  xy <- xy.coords(x, y)
  l <- length(xy$x)
  scanpath <- rep(NA, l)
  if (nearest) {
    for (i in 1:l) {
      dists <- sapply(rois, function(p, x2, y2) {
        x1 <- mean(p@coords[,"x"])
        y1 <- mean(p@coords[,"y"])
        xm <- abs(diff(range(p@coords[,"x"])))/2
        ym <- abs(diff(range(p@coords[,"y"])))/2
        list(p@ID, sqrt((x1-x2)**2 + (y1-y2)**2) - mean(xm,ym))
      }, x2=xy$x[i], y2=xy$y[i])
      scanpath[i] <- as.character(dists[1,which.min(dists[2,])])
    }
  } else {
    for (i in 1:length(rois)) {
      f <- which(point.in.polygon(xy$x,xy$y, rois[[i]]@coords[,"x"], rois[[i]]@coords[,"y"])!=0)
      if (length(f)>0)
        scanpath[f] <- rois[[i]]@ID
    }
  }
  new("Scanpath",scanpath,ROIs=rois)
}

#' Fortify Scanpath
#'
#' Method to convert a Scanpath object into a data frame useful for plotting.
#' 
#' @param scanpath a Scanpath object
#'
#' @return a data frame
#' 
# @export
#'  
fortify.Scanpath <- function(scanpath) {
  idx <- lapply(scanpath@.Data, function(x,y) which(x==y), lapply(scanpath@ROIs,slot,"ID"))
  df <- data.frame(id=scanpath@.Data,x=NA,y=NA)
  for (i in which(idx>0)) {
    df[i,"x"] = scanpath@ROIs[[idx[[i]]]]@center[1]
    df[i,"y"] = scanpath@ROIs[[idx[[i]]]]@center[2]
  }
  df
}