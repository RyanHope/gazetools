#' Get Scanpath
#'
#' Determines the scanpath given a set of fixations and a list of polygon ROIs
#' 
#' @param x,y the x and y coordinates of a set of points. Alternatively, a single argument x can be provided
#' @param begin,end the begin and end timestamp of each fixation, only used with DynamicROIs
#' @param rois an object of class \code{\linkS4class{ROIs}} or \code{\linkS4class{DynamicROIs}}
#' @param nearest if TRUE fixations not on an ROI will be moved to the nearest ROI
#' @param firstOnly if TRUE only DynamicROIs present at the start of a fixation are reported
#'
#' @importFrom sp point.in.polygon
#'
#' @return an object of class \code{\linkS4class{Scanpath}}
#'
#' @export
#' 
getScanpath <- function(x, y = NULL, begin = NULL, end = NULL, rois, nearest = FALSE, firstOnly = TRUE) {
  xy <- xy.coords(x, y)
  l <- length(xy$x)
  if (is(rois, "ROIs")) {
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
  } else if (is(rois, "DynamicROIs")) {
    scanpath <- c()
    N <- NULL
    if  (length(begin)==l || length(end)==l) {
      for (f in 1:l) {
        .rois <- data.frame()
        for (r in rev(levels(reorder(sapply(rois,function(x) x@ID),sapply(rois,function(x) x@layer))))) {
          hit <- 0
          z <- rois[[r]]@times
          w <- which(z$begin<end[f] & z$end>begin[f])
          if (length(w)>0) {
            for (i in w) {
              p <- point.in.polygon(xy$x[f],xy$y[f], rois[[r]][[i]][,"x"], rois[[r]][[i]][,"y"])
              if (p!=0) {
                .rois <- rbind(.rois, data.frame(begin=z[i,"begin"],id=r))
                hit <- hit + 1
              }
            }
          }
          if (hit>0) break
        }
        if (nrow(.rois)>0) {
          N <- c(N,length(rle(as.character(reorder(.rois$id,.rois$begin)))$values))
          # Its possible for there to be multiple DynamicROIs per fixation so what do we report for the scanpath?
          # I can think of 3 options, report them all in order of start time, report just the first, or report the longest one
          if (firstOnly)
            scanpath <- c(scanpath, as.character(.rois[which(.rois$begin==min(.rois$begin)),"id"]))
          else
            scanpath <- c(scanpath,rle(as.character(reorder(.rois$id,.rois$begin)))$values)
        } else {
          scanpath <- c(scanpath, NA)
        }
      }
      attr(scanpath,"N") <- N
    } else {
      stop("length of begin and end must equal length of x")
    }
  } else {
    stop("rois is not of class ROIs or DynamicROIs")
  }
  scanpath
}

#' Fortify Scanpath
#'
#' Method to convert a Scanpath object into a data frame useful for plotting.
#' 
#' @param scanpath a Scanpath object
#'
#' @return a data frame
#' 
#' @importFrom ggplot2 fortify
#' 
#' @export
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