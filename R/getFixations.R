#' Get Fixations
#' 
#' Extracts the coordinates of fixations and their durations from \code{\linkS4class{classify}} 
#' and \code{\linkS4class{pva}} objects
#' 
#' @param class an object of class \code{\linkS4class{classify}}
#' @param dpva an object of class \code{\linkS4class{pva}}
#' @param drop a boolean, if T (default) drop fixations at (0,0)
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
#' d.f <- getFixations(d.c, d.pva)
#' d.f
#'  
getFixations <- function(class, dpva, drop=T) {
  if (!is(class, "classify"))
    stop("class is not of class classify")
  if (!is(dpva, "pva"))
    stop("dpva is not of class pva")
  f <- subset(ddply(cbind(as.data.frame(dpva), as.data.frame(class)), .(fixation_ids),
                    function(d,t) data.frame(x=mean(d$x),
                                             y=mean(d$y),
                                             duration=nrow(d)*t),
                    t=1/dpva@samplerate), fixation_ids!=0)
  rownames(f) <- f$fixation_ids
  d <- f[,c("x","y","duration")]
  if (drop) {
    d <- d[d$x!=0 & d$y!=0,]
    rownames(d) <- NULL
  }
  d
}