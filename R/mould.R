#' Class "mould"
#'
#' A class to hold mould threshold information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}:}{a threshold value}
#'    \item{\code{thresholds}:}{vector of class \code{"numeric"}}
#'    \item{\code{resp1}:}{vector of class \code{"numeric"}}
#'    \item{\code{resp2}:}{vector of class \code{"numeric"}}
#'    \item{\code{gap}:}{vector of class \code{"numeric"}}
#'    \item{\code{type}:}{the type of threshold (velocity or acceleration)}
#'  }
#'
#' @docType class
#' @name mould-class
#' @rdname mould-class
#' @exportClass mould
setClass("mould", 
         representation(thresholds = "numeric",
                        resp1 = "numeric",
                        resp2 = "numeric",
                        gap = "numeric",
                        type = "character"), 
         contains = "numeric")

#' Plot mould
#' 
#' Plot the mould class
#' 
#' @param x an object of class \code{"mould"}
#' 
#' @docType methods
#' @import ggplot2
#' @rdname mould-methods
#' @name plot.mould
#' @export
#' @aliases plot,mould,missing-method
setMethod("plot", signature(x = "mould", y = "missing"),
          function(x, y, ...) {
            optimal<-as.numeric(x)
            dd <- data.frame(resp1 = slot(x, "resp1"),
                             resp2 = slot(x, "resp2"),
                             gap = slot(x, "gap"),
                             thresholds = slot(x, "thresholds"))
            if (x@type == "velocity")
              xlab <- "Speed threshold, deg/s"
            else if (x@type == "acceleration")
              xlab <- "Speed threshold, deg/s^2"
            else
              xlab <- "unknown"
            ggplot(dd, aes(x = thresholds, y = resp1)) + geom_area(fill = "gray") +
              ylab("Frequency of local speed maxima exceeding threshold") +
              xlab(labx) + 
              ylim(0, max(dd$resp1)) +
              geom_line(aes(x = thresholds, y = resp2), linetype = "dashed") +
              geom_line(aes(x = thresholds, y = gap)) + 
              geom_vline(xintercept = optimal, size = 1.5) +
              geom_text(data = data.frame(x = optimal, y = mean(dd$resp2)),
                        aes(x = x, y = y, hjust = -.15), label = as.character(sprintf("%.2f", optimal)))
          }
)