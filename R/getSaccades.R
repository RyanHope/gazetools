#' Get Saccades
#' 
#' Extracts the start point, end point, distance and duration of saccades from \code{classify}
#' and \code{pva} objects
#' 
#' @param class an object of class \code{classify}
#' @param dpva an object of class \code{pva}
#'
#' @importFrom plyr ddply .
#' @export
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' d.f <- getSaccades(d.c, d.pva)
#' d.f
#'  
getSaccades <- function(class, dpva) {
  if (!is(class, "classify"))
    stop("class is not of class classify")
  if (!is(dpva, "pva"))
    stop("dpva is not of class pva")
  f <- subset(ddply(cbind(as.data.frame(dpva), as.data.frame(class)), .(saccade_ids),
                    function(d,t) {
                      data.frame(duration=nrow(d)*t)
                    }, t=1/dpva@samplerate), saccade_ids!=0)
  rownames(f) <- f$saccade_ids
  f
}