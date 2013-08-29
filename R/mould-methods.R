utils::globalVariables(c("resp2","thresholds","resp1","gap"))

#' @importFrom methods setGeneric
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Plot mould
#' 
#' Plot the mould class
#' 
#' @param x an object of class \code{"mould"}
#' 
#' @docType methods
#' @importFrom ggplot2 ggplot aes geom_area ylab xlab geom_line ylim geom_text
#' @importFrom methods setMethod
#' @rdname mould-plot
#' @name plot.mould
#' @export
#' @aliases plot,mould,missing-method
#' 
#' @example example/pva.R
#' @example example/mouldThreshold.R
#' @example example/mould-plot.R
#' 
setMethod("plot", signature(x = "mould", y = "missing"),
          function(x, y, ...) {
            optimal <- as.numeric(x)
            dd <- data.frame(resp1 = slot(x, "resp1"),
                             resp2 = slot(x, "resp2"),
                             gap = slot(x, "gap"),
                             thresholds = slot(x, "thresholds"))
            if (x@type == "velocity") {
              xlab <- "Speed threshold, deg/s"
            } else if (x@type == "acceleration") {
              xlab <- "Speed threshold, deg/s^2"
            } else {
              xlab <- "unknown"
            }
            dd <- subset(dd, resp2>=0)
            ggplot(dd, aes(x = thresholds, y = resp1)) + geom_area(fill = "gray") +
              ylab("Frequency of local speed maxima exceeding threshold") +
              xlab(xlab) + 
              geom_line(aes(x = thresholds, y = resp2), linetype = "dashed") +
              geom_line(aes(x = thresholds, y = gap)) + 
              geom_vline(xintercept = optimal, size = 1.5) +
              geom_text(data = data.frame(x = optimal, y = mean(dd$resp2)),
                        aes(x = x, y = y, hjust = -.15), label = as.character(sprintf("%.2f", optimal))) +
              ylim(0, max(dd$resp1))
          }
)