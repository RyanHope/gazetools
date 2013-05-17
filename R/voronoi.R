require(rgeos)
require(maptools)
require(deldir)
require(sp)
require(modeest)
require(plyr)

#' Voronoi Polygons
#'
#' Computes Voronoi polygons based on a set of x,y coordinates
#' 
#' @param x a matrix of x,y cooridinates
#' @param rw the coordinates of the corners of the rectangular window enclosing the voronoi cells, in the order (xmin, xmax, ymin, ymax).
#'
#' @import rgeos
#' @import maptools
#' @import deldir
#' @import sp
#' @import modeest
#'
#' @export
voronoi_polygons <- function(x, rw) {
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  z <- deldir(crds[,1], crds[,2], rw=rw, suppressMsge=T)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  d <- data.frame(x=crds[,1],
                  y=crds[,2],
                  row.names=sapply(slot(SP, 'polygons'), 
                                   function(x) slot(x, 'ID')))
  SpatialPolygonsDataFrame(SP, data=d)
}

#' Class "voronoi_skewness"
#'
#' A class to hold Voronoi skewness information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}:}{the skewness of the area of the voronoi cells}
#'    \item{\code{polygons}:}{an object of class \code{SpatialPolygonsDataFrame}}
#'  }
#'
#' @importClassesFrom sp SpatialPolygonsDataFrame
#'
#' @docType class
#' @name voronoi_skewness-class
#' @rdname voronoi_skewness-class
#' @exportClass voronoi_skewness
setClass("voronoi_skewness", 
         representation(polygons = "SpatialPolygonsDataFrame"),
         contains = "numeric")

#' Plot voronoi_skewness
#' 
#' Plot the voronoi_skewness class
#' 
#' @param x an object of class \code{"voronoi_skewness"}
#' 
#' @docType methods
#' @import ggplot2
#' @rdname pva-plot
#' @name plot.voronoi_skewness
#' @export
#' @aliases plot,voronoi_skewness,missing-method
setMethod("plot", signature(x = "voronoi_skewness", y = "missing"), 
          function(x, y) {
            d <- x@polygons
            d@data$id = rownames(d@data)
            d.points = fortify(d, region="id")
            d.df = join(d.points, d@data, by="id")
            ggplot(d.df, aes(long,lat,group=group)) +
              geom_path() +
              coord_equal(ratio=1, xlim=c(315,1365), ylim=c(0,1050)) +
              geom_point(aes(x=x,y=y),data=ddply(d.df, .(group), function(d) data.frame(x=mean(d$x),y=mean(d$y))),size=3) +
              theme(panel.background = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank())
          })



#' Voronoi Skewness
#'
#' Given a matrix of x,y coordinates computes the skewness of the area of the voronoi cells
#' 
#' @param d a matrix of x,y coordinates
#' @param rw the coordinates of the corners of the rectangular window enclosing the voronoi cells, in the order (xmin, xmax, ymin, ymax).
#'
#' @export
voronoi_skewness <- function(d, rw) {
  v <- voronoi_polygons(d, rw)
  s <- as.numeric(skewness(sapply(v@polygons, function(x) x@area)))
  new("voronoi_skewness", s, polygons=v)
}

#' Voronoi Cell Geom for ggplot2
#' 
#' @param d an object of class \code{SpatialPolygonsDataFrame}
#' @param ... extra arguments passed on to geom_path
#'
#' @import plyr
#' @import ggplot2
#' @export
geom_voronoi <- function(d, ...) {
  if (cladd(d) == "SpatialPolygonsDataFrame") {
    d@data$id = rownames(d@data)
    d.points = fortify(d, region="id")
    d.df = join(d.points, d@data, by="id")
    geom_path(aes(long,lat,group=group), d.df, ...)
  } else
    stop("d is wrong class")
}