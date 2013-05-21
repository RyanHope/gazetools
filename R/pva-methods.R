utils::globalVariables(c("variable","value","intercept"))

#' Coerce object of class \code{\linkS4class{pva}} to a Data Frame
#' 
#' @rdname pva-as.data.frame
#' @aliases as.data.frame,pva,missing,missing-method
#' @name as.data.frame.pva
#' @export
setMethod("as.data.frame", signature(x = "pva", row.names = "missing", optional = "missing"),
          function(x) {
            data.frame(time = x@time, ez = x@ez, ex = x@ex, ey = x@ey, x = x@x, y = x@y,
                       sx = x@sx, sy = x@sy, xa = x@xa, ya = x@ya, v = x@v, a = x@a, 
                       rx = x@rx, ry = x@ry, samplerate = x@samplerate)
          }
)

setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Plot pva
#' 
#' Plot the pva class
#' 
#' @param x an object of class \code{\linkS4class{pva}}
#' 
#' @docType methods
#' @import reshape
#' @import ggplot2
#' @rdname pva-plot
#' @name plot.pva
#' @export
#' @aliases plot,pva,missing-method
#' 
#' @example example/pva.R
#' @example example/pva-plot.R
#' 
setMethod("plot", signature(x = "pva", y = "missing"), function(x, y) pva.plot(x, NULL))

#' @param y an object of class \code{\linkS4class{classify}} (optional)
#' 
#' @docType methods
#' @import reshape
#' @import ggplot2
#' @rdname pva-plot
#' @name plot.pva
#' @export
#' @aliases plot,pva,classify-method
#' 
#' @example example/classify.VA.R
#' @example example/pva-classify-plot.R
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
  if (!is.null(y) & class(y)=="classify") {
    thresholds <- data.frame(variable=levels(d$variable))
    if (length(y@thresholds) > 1) 
      thresholds$intercept <- c(NA,NA,y@thresholds)
    else
      thresholds$intercept <- c(NA,NA,y@thresholds,NA)
    p <- ggplot(d) + geom_point(aes(x=time, y=value, color=class)) + 
      geom_hline(data=thresholds,aes(yintercept=intercept)) +
      scale_color_manual(values=c("black", "red"))
  } else
    p <- ggplot(d) + geom_point(aes(x=time, y=value))
  p + facet_grid(variable~., scales="free_y") + ylab("") + xlab("Time (s)") +
    theme(legend.position = "top")
}