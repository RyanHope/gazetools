utils::globalVariables(c("y"))

#' Extent (range) of fixations
#'
#' Calculates the distance between fixations in the horizontal and vertical meridians
#'   
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' 
#' @return a list with two values, the horizontal (x) and vertical (y) range of points
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
#' extent(d.f[,c("fixation.x","fixation.y")])
#' 
extent <- function(x) {
  list(x=max(x[,1])-min(x[,1]),y=max(x[,2])-min(x[,2]))
}