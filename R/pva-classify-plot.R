#' Plot a 'pva' Object
#' 
#' Plot an object of class 'pva'
#' 
#' @param x an object of class \code{\link[=pva-class]{pva}}
#' 
#' @export
#' 
#' @examples
#' data(smi)
#' # Raw (unclassified) position, velocity and acceleration plot
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                       smi_ezl, smi_exl, smi_eyl))
#' plot(d.pva)
#' 
setMethod("plot", signature(x = "pva", y = "missing"), function(x, y, ...) pva.plot(x, NULL, ...))

#' Plot a 'pva' Object with Classified Samples
#' 
#' Plot an object of class 'pva' with classification information from an object of class 'classify'
#' 
#' @param x an object of class \code{\link[=pva-class]{pva}}
#' @param y an object of class \code{\link[=classify-class]{classify}} (optional)
#' 
#' @export
#' 
#' @examples
#' data(smi)
#' # Classified position, velocity and acceleration plot
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.VA(d.pva@@v, d.pva@@a)
#' plot(d.pva, d.c)
#' 
#' # Classified position, velocity and acceleration plot
#' # with blinks removed
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl, pupil=smi_dyl))
#' d.c <- classify.VA(d.pva@@v, d.pva@@a, blinks=d.pva@@blinks)
#' plot(d.pva, d.c)
#' 
setMethod("plot", signature(x = "pva", y = "classify"), function(x, y, ...) pva.plot(x, y, ...))

#' @autoImports
pva.plot <- function(x, y, ...)
{
  d <- as.data.frame(x)
  xlims <- c(min(d$time),max(d$time))
  if (!is.null(y) & class(y)=="classify") {
    d$class <- factor(y)
    d <- melt(d, id=c("time","class"))
  } else
    d <- melt(d, id=c("time"))
  d$variable <- factor(d$variable,labels=c("Gaze X", "Gaze Y", "Velocity", "Acceleration"))
  if (!is.null(y) & class(y)=="classify") {
    d$class <- factor(d$class, levels=c("FIXATION","SACCADE","BLINK"))
    thresholds <- data.frame(variable=levels(d$variable))
    if (length(y@thresholds) > 1) 
      thresholds$intercept <- c(NA,NA,y@thresholds)
    else
      thresholds$intercept <- c(NA,NA,y@thresholds,NA)
    thresholds$x <- xlims[1]
    thresholds$xend <- xlims[2]
    p <- ggplot(d) + geom_point(aes_string(x="time", y="value", color="class")) + 
      geom_segment(data=thresholds,aes_string(y="intercept",yend="intercept",x="x",xend="xend"), na.rm=T) +
      scale_color_manual(values=c("black", "red", "blue"))
  } else
    p <- ggplot(d) + geom_point(aes_string(x="time", y="value"))
  p + facet_grid(as.formula("variable~."), scales="free_y") + ylab("") + xlab("Time (s)") +
    theme(legend.position = "top") + coord_cartesian(xlim=xlims)
}