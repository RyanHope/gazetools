utils::globalVariables(c("glissade_ids"))

#' Get Glissades
#' 
#' Extracts the start point, end point, distance and duration of glissades from \code{classify}
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
#'                        smi_ezl, smi_exl, smi_eyl, pupil=smi_dyl))
#' d.c <- classify.VI(d.pva@@v, sigma=4, blinks=d.pva@@blinks)
#' d.f <- getGlissades(d.c, d.pva)
#' d.f
#'  
getGlissades <- function(class, dpva) {
  if (!is(class, "classify"))
    stop("class is not of class classify")
  if (!is(dpva, "pva"))
    stop("dpva is not of class pva")
  d1 <- as.data.frame(dpva)
  d2 <- as.data.frame(class)
  f <- subset(ddply(cbind(d1,d2), .(glissade_ids),
                    function(d,t,a) {
                      n <- nrow(d)
                      data.frame(glissade.duration=n*t,
                                 glissade.x1=d[1,"sx"],
                                 glissade.y1=d[1,"sy"],
                                 glissade.x2=d[n,"sx"],
                                 glissade.y2=d[n,"sy"],
                                 glissade.amplitude=subtended_angle(d[1,"sx"],d[1,"sy"],d[n,"sx"],d[n,"sy"],a$rx,a$ry,a$sw,a$sh,d[1,"ez"],d[n,"ex"],d[n,"ey"])
                      )
                    }, t=1/dpva@samplerate,a=attributes(d1)[tail(names(attributes(d1)),6)]), glissade_ids!=0)
  rownames(f) <- NULL
  colnames(f)[1] <- "glissade.ids"
  f
}