#' Nearest Neighbor Index
#' 
#' Calculates the dispersion of a set of points.
#' 
#' @param x coordinate vectors of points. This can be specified as a 2-column matrix x, 
#' a list x with two components
#' 
#' @return The dispersion of a set of points; this value is equal to 1 when the distribution 
#' is random. Values lower than 1 suggest grouping, whereas values higher than 1 suggest 
#' regularity (i.e. the point pattern is dispersed in a non-random way). 
#'
#' @importFrom spatstat nndist.default
#' @export
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' d.f <- getFixations(d.c, d.pva)
#' d.nni <- nni(d.f[,c("fixation.x","fixation.y")])
#' str(d.nni)
#' 
nni <- function(x) {
  mean(nndist.default(x))/(.5*sqrt(chull_area(x)/nrow(x)))
}