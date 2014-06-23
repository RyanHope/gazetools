#' Raster Plot
#'
#' A wrapper around a few ggplot2 functions that creates a base plot layer out of a raster image.
#' Coordinates are restricted to values within the raster image and the aspect ratio is fixed.
#' 
#' @param raster a rasterized image
#' @param xlim a vector of length 2 that contains the min and max horizontal bounds of the image
#' @param ylim a vector of length 2 that contains the min and max vertical bounds of the image
#' 
#' @importFrom ggplot2 ggplot annotation_raster coord_fixed aes
#' 
#' @export
#' 
#' @examples
#' data(logo)
#' rplot(as.raster(logo))
#'
rplot <- function(raster, xlim=NULL, ylim=NULL) {
  if (is.null(xlim))
    xlim <- c(0,ncol(raster))
  if (is.null(ylim))
    ylim <- c(0,nrow(raster))
  raster <- raster[seq(ylim[1]+1,ylim[2]),seq(xlim[1]+1,xlim[2])]
  ggplot(data=data.frame(x=xlim,y=ylim),aes(x=x,y=y)) + 
    annotation_raster(raster,xlim[1],xlim[2],ylim[1],ylim[2],TRUE) + 
    coord_fixed(xlim=xlim,ylim=ylim)
}