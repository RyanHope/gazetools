#' @importFrom methods setGeneric
setGeneric("mouldThreshold", function(dpva, ...) standardGeneric("mouldThreshold"))

#' Mould Threshold
#' 
#' Uses the Mould algorithm to determine the optimal velocity or acceleration threshold for
#' classifying raw gaze data.
#' 
#' @template pva
#' @param type 'velocity' or 'acceleration'
#' 
#' @return an object of class \code{\linkS4class{mould}}
#' 
#' @docType methods
#' @rdname pva-mouldThreshold
#' @aliases mouldThreshold,pva-method
#' @export
#' @importFrom methods setMethod
#' 
#' @example example/pva.R
#' @example example/mouldThreshold.R
#' @example example/mouldThreshold-out.R
#' 
setMethod("mouldThreshold", signature(dpva = "pva"), 
          function(dpva, type = "velocity") {
            if (type == "velocity")
              d <- dpva@v
            else if (type == "acceleration")
              d <- dpva@a
            else
              stop("invalid type")
            peaks <- d[local_maxima(d)]
            r <- range(peaks)
            thresholds <- seq(r[1], r[2], length.out = dpva@samplerate)
            if (type == "velocity")
              l <- length(thresholds)
            else
              l <- length(thresholds)^2
            resp1 <- sapply(thresholds, function(x) {length(which(peaks > x))})
            resp2 <- sapply(thresholds, function(x) {length(peaks) * (1 - x / l)})
            gap <- resp2 - resp1
            f <- .001
            gap <- lowess(gap, f = f)$y
            while (length(local_maxima(gap)) > 1) {
              f <- f + .001
              gap <- lowess(resp2 - resp1, f = f)$y
            }
            optimal <- thresholds[local_maxima(gap)]
            new("mould", round(optimal, 2), thresholds = thresholds, resp1 = resp1, resp2 = resp2, gap = gap, type = type)
          })