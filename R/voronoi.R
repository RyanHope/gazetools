utils::globalVariables(c("long","lat","group"))

#' Voronoi Polygons
#'
#' Computes Voronoi polygons based on a set of x,y coordinates
#' 
#' @param x a matrix of x,y cooridinates
#' @param rw the coordinates of the corners of the rectangular window enclosing the voronoi cells, in the order (xmin, xmax, ymin, ymax).
#'
#' @importFrom sp Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom deldir deldir tile.list
#'
#' @return an object of class \code{\linkS4class{SpatialPolygonsDataFrame}}
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
#' d.vp <- voronoi_polygons(d.f[,c("x","y")], c(315,1365,0,1050))
#' str(d.vp)
#' 
voronoi_polygons <- function(x, rw) {
  crds <- x
  z <- deldir(crds[,1], crds[,2], rw=rw, suppressMsge=T)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  r <- as.numeric(sapply(SP@polygons, function(x) slot(x, 'ID')))
  d <- data.frame(x=crds[r,1],
                  y=crds[r,2],
                  row.names=r)
  SpatialPolygonsDataFrame(SP, data=d)
}

#' Class "voronoi_skewness"
#'
#' A class to hold Voronoi skewness information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}}{the skewness of the area of the voronoi cells}
#'    \item{\code{polygons}}{an object of class \code{\linkS4class{SpatialPolygonsDataFrame}}}
#'  }
#'
#' @importClassesFrom sp Line CRS Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom methods setClass
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
#' Creates a scatter plot of fixations with a Voronoi tessellation overlay
#' 
#' @param x an object of class \code{\linkS4class{voronoi_skewness}}
#' 
#' @docType methods
#' @importFrom ggplot2 ggplot geom_path coord_equal geom_point theme element_blank aes
#' @importFrom plyr ddply .
#' @importFrom methods setMethod
#' @rdname voronoi_skewness-plot
#' @name plot.voronoi_skewness
#' @export
#' @aliases plot,voronoi_skewness,missing-method
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' d.f <- getFixations(d.c, d.pva)
#' d.vs <- voronoi_skewness(d.f[,c("x","y")], c(315,1365,0,1050))
#' plot(d.vs)
#' 
setMethod("plot", signature(x = "voronoi_skewness", y = "missing"), function(x, y, ...) voronoi_skewness.plot(x, y, ...))

voronoi_skewness.plot <- function(x, y, ...) {
  d.df <- suppressMessages(fortify(x@polygons))
  ggplot(d.df, aes(long,lat,group=group)) +
    geom_path(na.rm=T) +
    coord_equal(ratio=1, xlim=c(315,1365), ylim=c(0,1050)) +
    geom_point(aes(x=x,y=y),data=ddply(d.df, .(group), 
                                       function(d) data.frame(x=mean(d$long),
                                                              y=mean(d$lat))),
               size=3) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}

#' Voronoi Skewness
#'
#' Given a matrix of x,y coordinates computes the skewness of the area of the voronoi cells
#' 
#' @param d a matrix of x,y coordinates
#' @param rw the coordinates of the corners of the rectangular window enclosing the voronoi cells, in the order (xmin, xmax, ymin, ymax).
#'
#' @return an object of class \code{\linkS4class{voronoi_skewness}}
#' 
#' @importFrom modeest skewness
#' 
#' @example example/pva.R
#' @example example/classify.V.R
#' @example example/getFixations.R
#' @example example/voronoi_skewness.R
#' @example example/voronoi_skewness-out.R
#'
#' @export
voronoi_skewness <- function(d, rw) {
  v <- voronoi_polygons(d, rw)
  s <- as.numeric(skewness(sapply(v@polygons, function(x) x@area)))
  new("voronoi_skewness", s, polygons=v)
}

#' Voronoi Cell Geom for ggplot2
#' 
#' @param d an object of class \code{\linkS4class{SpatialPolygonsDataFrame}}
#' @param ... extra arguments passed on to geom_path
#'
#' @importFrom ggplot2 geom_path fortify aes
#' @export
geom_voronoi <- function(d, ...) {
  suppressMessages(geom_path(aes(long,lat,group=group), fortify(d), ...))
}