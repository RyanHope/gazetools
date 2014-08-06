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
#' @return an object of class \code{\linkS4class{ROIs}}
#'
#' @rdname gridded_rois
#' @export
#' 
#' @examples
#' rois <- gridded_rois(315,1365,0,1050,nrow=11,ncol=11)
#' str(rois)
#' 
gridded_rois <- function(xmin, xmax, ymin, ymax, ncol, nrow) {
  deltax <- (xmax-xmin)/ncol
  deltay <- (ymax-ymin)/nrow
  rois <- list()
  for (x in 0:(ncol-1)) {
    for (y in 0:(nrow-1)) {
      id <- sprintf("%d%s",x,LETTERS[(y+1)])
      rois[[id]] <- ROI(data.frame(x=c((x)*deltax,(x+1)*deltax,(x+1)*deltax,(x)*deltax, (x)*deltax),
                                   y=c((y)*deltay,(y)*deltax,(y+1)*deltay,(y+1)*deltay, (y)*deltay)), id)
    }
  }
  ROIs(rois)  
}