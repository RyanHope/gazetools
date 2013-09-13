#' PNG geom
#'
#' Creates a ggplot layer that contains a png image
#' 
#' @param file the path to a png image
#' @param xlim a vector of length 2 that contains the min and max horizontal bounds of the image
#' @param ylim a vector of length 2 that contains the min and max vertical bounds of the image
#' 
#' @import ggplot2
#' @importFrom png readPNG
#' 
#' @export
#'
geom_png <- function(file, xlim=NULL, ylim=NULL) {
  img <- as.raster(readPNG(img, FALSE))
  if (is.null(xlim))
    xlim <- c(0,ncol(img))
  if (is.null(ylim))
    ylim <- c(0,nrow(img))
  img <- img[seq(ylim[1]+1,ylim[2]),seq(xlim[1]+1,xlim[2])]
  annotation_raster(img,xlim[1],xlim[2],ylim[1],ylim[2],TRUE)
}