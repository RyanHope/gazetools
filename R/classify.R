require(plyr)
require(ggplot2)

#' Class "classify"
#'
#' A class to hold gaze data classifications
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}:}{vector of class \code{"character"}, containing the gaze sample classification}
#'    \item{\code{fixation_ids}:}{vector of class \code{"numeric"}, containing unique fixation ids}
#'    \item{\code{saccade_ids}:}{vector of class \code{"numeric"}, containing unique saccade ids}
#'    \item{\code{algorithm}:}{the algorithm used to classify the gaze data}
#'    \item{\code{thresholds}:}{the threshold settings for the classification algorithm}
#'  }
#'
#' @docType class
#' @name classify-class
#' @rdname classify-class
#' @exportClass classify
setClass("classify", 
         representation(fixation_ids = "numeric", saccade_ids = "numeric",
                        algorithm="character", thresholds="numeric"),
         contains="character")

#' Coerce object of class \code{classify} to a Data Frame
#' 
#' @rdname classify-as.data.frame
#' @aliases as.data.frame,classify,missing,missing-method
#' @name classify.as.data.frame
#' @export
setMethod("as.data.frame", signature(x = "classify", row.names = "missing", optional = "missing"),
          function(x) {
            data.frame(class = x@.Data, fixation_ids = x@fixation_ids, saccade_ids = x@saccade_ids)
          }
)

#' Plot classify
#' 
#' Plot the classify class
#' 
#' @param x an object of class \code{\linkS4class{classify}}
#' @param y an object of class \code{\linkS4class{pva}}
#' 
#' @docType methods
#' @import plyr
#' @import ggplot2
#' @rdname classify-plot
#' @name plot.classify
#' @export
#' @aliases plot,classify,pva-method
setMethod("plot", signature(x = "classify", y = "pva"), function(x, y) classify.plot(x, y))

classify.plot <- function(x, y = NULL, reverse_y = TRUE)
{
  classify_obj <- x
  pva_obj <- y
  
  tmp <- cbind(as.data.frame(pva_obj), as.data.frame(classify_obj))
  
  tmp2 <- ddply(subset(tmp, fixation_ids!=0), .(fixation_ids), function(x) {
    data.frame(x=mean(x$x),y=mean(x$y),dur=nrow(x) * (1/pva_obj@samplerate) * 1000)
  })
  
  p <- ggplot(tmp2, aes(x=x,y=y)) + 
    geom_path(linetype="dotted") + 
    geom_point(aes(size=dur),alpha=.5) +
    coord_equal(ratio=1, xlim=c(0,pva_obj@rx), ylim=c(0,pva_obj@ry)) +
    labs(size = "Fixation Duration (ms)", x = element_blank(), y = element_blank()) +
    theme(legend.position = "top") +
    scale_size_continuous(range = c(3,10)) +
    scale_x_continuous(breaks=seq(0,pva_obj@rx,pva_obj@rx*.1))
  if (reverse_y)
    p <- p + scale_y_reverse(breaks=seq(0,pva_obj@ry,pva_obj@ry*.1))
  else
    p <- p + scale_y_continuous(breaks=seq(0,pva_obj@ry,pva_obj@ry*.1))
  
  p
}

#' Get Fixations
#' 
#' Extracts fixation locations from \code{classify} and \code{pva} objects.
#' 
#' @template class
#' @template pva
#' 
#' @export
#' @docType methods
#' @rdname classify-getFixations
setGeneric("getFixations", function(class, dpva) standardGeneric("getFixations"))

#' @import plyr
#' @rdname classify-getFixations
#' @aliases getFixations,classify,pva-method
#' @export
setMethod("getFixations", signature(class = "classify", dpva = "pva"), 
          function(class, dpva) {
            f <- subset(ddply(cbind(as.data.frame(dpva), as.data.frame(class)), .(fixation_ids),
                              function(d,t) data.frame(x=mean(d$x),
                                                     y=mean(d$y),
                                                     duration=nrow(d)*t),
                              t=1/dpva@samplerate), fixation_ids!=0)
            rownames(f) <- f$fixation_ids
            f[,c("x","y","duration")]
          })