dirichlet_tessellation <- voronoi_tessellation <- function(x, y=NULL, xrange, yrange) {
  xy <- xy.coords(x, y)
  p <- ppp(xy$x, xy$y, xrange=xrange, yrange=yrange)
  dirichlet(p)
}

delaunay_tessellation <- function(x, y=NULL, xrange, yrange) {
  xy <- xy.coords(x, y)
  p <- ppp(xy$x, xy$y, xrange=xrange, yrange=yrange)
  delaunay(p)
}

skewness.tess <- function(x, ...) {
  as.numeric(skewness(unlist(tile.areas(x)), ...))
}

fortify.tess <- function(tl) {
  do.call("rbind",lapply(tl, function(x) {
    df <- data.frame(id=x$ptNum,
                     x=x$x,xend=c(x$x[-1],x$x[1]),
                     y=x$y,yend=c(x$y[-1],x$y[1]),
                     bp1=x$bp,bp2=c(x$bp[-1],x$bp[1]))
    df$bp <- df$bp1 & df$bp2
    df$bp1 <- NULL
    df$bp2 <- NULL
    df
  }))
}

#' VoronoiPolygons
#'
#' Creates an object of class VoronoiPolygons
#' 
#' @param x a matrix of x,y cooridinates
#' @param rw the coordinates of the corners of the rectangular window enclosing the voronoi cells, in the order (xmin, xmax, ymin, ymax).
#'
#' @import sp
#' @importFrom deldir deldir tile.list
#' @importFrom modeest skewness
#'
#' @return an object of class \code{\linkS4class{Voronoi}}
#'
#' @export
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' d.f <- getFixations(d.c, d.pva)
#' d.vp <- VoronoiPolygons(d.f[,c("x","y")], c(315,1365,0,1050))
#' str(d.vp)
#' 
VoronoiPolygons <- function(x, rw) {
  crds <- x
  z <- deldir(crds[,1], crds[,2], rw=rw, suppressMsge=T)
  w <- tile.list(z)
  rois <- ROIs(lapply(w, function(w) {
    pcrds <- cbind(w$x, w$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    ROI(pcrds, as.character(w$ptNum))
  }))
  new("VoronoiPolygons", rois, skewness=as.numeric(skewness(sapply(rois, function(x) x@area))))
}

#' Plot VoronoiPolygons
#' 
#' Plots Voronoi Tessellation
#' 
#' @param x an object of class \code{\linkS4class{VoronoiPolygons}}
#' 
#' @docType methods
#' @import ggplot2
#' @importFrom plyr ddply .
#' @importFrom methods setMethod
#' @rdname VoronoiPolygons-plot
#' @name plot.VoronoiPolygons
#' @export
#' @aliases plot,VoronoiPolygons,missing-method
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' d.f <- getFixations(d.c, d.pva)
#' d.vp <- VoronoiPolygons(d.f[,c("x","y")], c(315,1365,0,1050))
#' plot(d.vp)
#' 
setMethod("plot", signature(x = "VoronoiPolygons", y = "missing"), function(x, y, ...) VoronoiPolygons.plot(x, y, ...))

VoronoiPolygons.plot <- function(x, y, ...) {
  ggplot() +
    geom_path(aes(x,y,group=id), x, na.rm=T) +
    coord_equal(ratio=1, xlim=c(315,1365), ylim=c(0,1050)) +
    geom_point(aes(x=x,y=y), data.frame(x=as.numeric(sapply(x,function(x) x@center[1])),
                                        y=as.numeric(sapply(x,function(x) x@center[2]))),
               size=3) +
    theme_empty()
}

