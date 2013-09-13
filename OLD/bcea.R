#' Bivariate contour ellipse area (BCEA)
#' 
#' Calculates the area of an ellipse that encompasses a given proportion 
#' of points in a data set.
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' @param k a proportion of points in a data set
#' 
#' @return an area (pixels^2)
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
#' d.bcea <- bcea(d.f[,c("x","y")])
#' str(d.bcea)
#' 
bcea <- function(x, k=.90) {
  2 * k * pi * sd(x[,1]) * sd(x[,2]) * sqrt((1 - cor(x[,1],x[,2])^2))
}