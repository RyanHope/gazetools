utils::globalVariables(c("saccade_ids"))

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
  d1 <- as.data.frame(dpva)
  d2 <- as.data.frame(class)
  if (length(unique(class@saccade_ids))<2) return(NULL)
  f <- ddply(cbind(d1,d2), .(saccade_ids),
             function(d,t,a) {
               n <- nrow(d)
               data.frame(saccade.duration=n*t,
                          saccade.x1=d[1,"sx"],
                          saccade.y1=d[1,"sy"],
                          saccade.x2=d[n,"sx"],
                          saccade.y2=d[n,"sy"],
                          saccade.amplitude=subtended_angle(d[1,"sx"],d[1,"sy"],d[n,"sx"],d[n,"sy"],a$rx,a$ry,a$sw,a$sh,d[1,"ez"],d[n,"ex"],d[n,"ey"]),
                          saccade.peak.velocity=max(d$v),
                          saccade.peak.acceleration=max(d$a)
                          )
               }, t=1/dpva@samplerate,a=attributes(d1)[tail(names(attributes(d1)),6)])
  f <- f[f$saccade_ids != 0, ]
  rownames(f) <- NULL
  colnames(f)[1] <- "saccade.ids"
  f
}