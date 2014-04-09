#' Get Fixations
#' 
#' Extracts the coordinates of fixations and their durations from \code{classify}
#' and \code{pva} objects
#' 
#' @param class an object of class \code{classify}
#' @param dpva an object of class \code{pva}
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
  d1 <- as.data.frame(dpva)
  d2 <- as.data.frame(class)
  f <- subset(ddply(cbind(d1,d2), .(fixation_ids),
                    function(d,t) data.frame(fixation.x=mean(d$x),
                                             fixation.y=mean(d$y),
                                             fixation.duration=nrow(d)*t,
                                             fixation.velocity=mean(d$v),
                                             quality=mean(d$quality)),
                    t=1/dpva@samplerate), fixation_ids!=0)
  if (drop)
    f <- f[f$fixation.x!=0 & f$fixation.y!=0,]
  rownames(f) <- NULL
  colnames(f)[1] <- "fixation.ids"
  f
}