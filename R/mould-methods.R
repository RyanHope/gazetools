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
#' @importFrom ggplot2 ggplot aes geom_area ylab xlab geom_line geom_text coord_cartesian
#' @importFrom methods setMethod
#' @rdname mould-plot
#' @name plot.mould
#' @export
#' @aliases plot,mould,missing-method
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.mt <- mouldThreshold(d.pva@@v, d.pva@@samplerate)
#' plot(d.mt)
#' 
setMethod("plot", signature(x = "mould", y = "missing"), function(x, ...) mould.plot(x, ...))

mould.plot <- function(x, ...) {
  optimal <- as.numeric(x)
  dd <- data.frame(resp1 = slot(x, "resp1"),
                   resp2 = slot(x, "resp2"),
                   gap = slot(x, "gap"),
                   thresholds = slot(x, "thresholds"))
  dd <- subset(dd, resp2>=0 & gap>=0)
  ggplot(dd, aes(x = thresholds, y = resp1)) + geom_area(fill = "gray") +
    ylab("Frequency of local speed maxima exceeding threshold") +
    xlab("Speed threshold, deg/s") + 
    geom_line(aes(x = thresholds, y = resp2), linetype = "dashed") +
    geom_line(aes(x = thresholds, y = gap)) + 
    geom_vline(xintercept = optimal, size = 1.5) +
    geom_text(data = data.frame(x = optimal, y = mean(dd$resp2)),
              aes(x = x, y = y, hjust = -.15), label = as.character(sprintf("%.2f", optimal))) +
    coord_cartesian(ylim=c(0, max(dd$resp1)), xlim=c(0, max(dd$thresholds)))
}