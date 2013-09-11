utils::globalVariables(c("variable","value","intercept","xend"))

as.data.frame.pva <- function(x, row.names=NULL, optional=FALSE, ...) {
  data.frame(time = x@time, ez = x@ez, ex = x@ex, ey = x@ey, x = x@x, y = x@y,
             sx = x@sx, sy = x@sy, xa = x@xa, ya = x@ya, v = x@v, a = x@a, 
             rx = x@rx, ry = x@ry, samplerate = x@samplerate)
}

#' as("pva", "data.frame")
#'
#' @name as
#' @family classify
#'
setAs("pva","data.frame", function(from) as.data.frame.pva(from))

#' Plot pva
#' 
#' Plot the pva class
#' 
#' @param x an object of class \code{\linkS4class{pva}}
#' 
#' @docType methods
#' @importFrom reshape2 melt
#' @import ggplot2
#' @importFrom methods setMethod
#' @rdname pva-plot
#' @name plot.pva
#' @export
#' @aliases plot,pva,missing-method
#' 
#' @examples
#' # Raw (unclassified) position, velocity and acceleration plot
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                       smi_ezl, smi_exl, smi_eyl))
#' plot(d.pva)
#' 
setMethod("plot", signature(x = "pva", y = "missing"), function(x, y) pva.plot(x, NULL))

#' @param y an object of class \code{\linkS4class{classify}} (optional)
#' 
#' @docType methods
#' @importFrom reshape2 melt
#' @import ggplot2
#' @importFrom methods setMethod
#' @rdname pva-plot
#' @name plot.pva
#' @export
#' @aliases plot,pva,classify-method
#' 
#' @examples
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
setMethod("plot", signature(x = "pva", y = "classify"), function(x, y) pva.plot(x, y))

pva.plot <- function(x, y, ...)
{
  d <- as.data.frame(x)
  if (!is.null(y) & class(y)=="classify") {
    d$class <- factor(y)
    d <- melt(d, id=c("time","class"))
  } else
    d <- melt(d, id=c("time"))
  d <- subset(d, variable=="sx" | variable=="sy" | variable=="v" | variable=="a")
  d$variable <- factor(d$variable,labels=c("Gaze X", "Gaze Y", "Velocity", "Acceleration"))
  xlims <- c(min(x@time),max(x@time))
  if (!is.null(y) & class(y)=="classify") {
    d$class <- factor(d$class, levels=c("FIXATION","SACCADE","BLINK"))
    thresholds <- data.frame(variable=levels(d$variable))
    if (length(y@thresholds) > 1) 
      thresholds$intercept <- c(NA,NA,y@thresholds)
    else
      thresholds$intercept <- c(NA,NA,y@thresholds,NA)
    thresholds$x <- xlims[1]
    thresholds$xend <- xlims[2]
    p <- ggplot(d) + geom_point(aes(x=time, y=value, color=class)) + 
      geom_segment(data=thresholds,aes(y=intercept,yend=intercept,x=x,xend=xend), na.rm=T) +
      scale_color_manual(values=c("black", "red", "blue"))
  } else
    p <- ggplot(d) + geom_point(aes(x=time, y=value))
  p + facet_grid(variable~., scales="free_y") + ylab("") + xlab("Time (s)") +
    theme(legend.position = "top") + coord_cartesian(xlim=xlims)
}