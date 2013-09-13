utils::globalVariables(c("fixation_ids","dur"))

as.data.frame.classify <- function(x, row.names=NULL, optional=FALSE, ...) {
  data.frame(class = x@.Data, fixation_ids = x@fixation_ids, saccade_ids = x@saccade_ids)
}

#' as("classify", "data.frame")
#'
#' @name as
#' @family classify
#'
setAs("classify","data.frame", function(from) as.data.frame.classify(from))


#' Plot classify
#' 
#' Creates a scatter plot of the scanpath between each fixation. Fixation durations are indicated by the size of the fixation point.
#' 
#' @param x an object of class \code{\linkS4class{classify}}
#' @param y an object of class \code{\linkS4class{pva}}
#' 
#' @docType methods
#' @importFrom plyr ddply .
#' @import ggplot2
#' @importFrom methods setMethod
#' @rdname classify-methods
#' @name plot.classify
#' @export
#' @aliases plot,classify,pva-method
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl, pupil=smi_dxl))
#' d.c <- classify.VA(d.pva@@v, d.pva@@a, blinks=d.pva@@blinks)
#' plot(d.c, d.pva)
#' 
setMethod("plot", signature(x = "classify", y = "pva"), function(x, y, ...) classify.plot(x, y, ...))

classify.plot <- function(x, y, reverse_y = FALSE)
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
          