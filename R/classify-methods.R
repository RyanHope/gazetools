utils::globalVariables(c("fixation_ids","dur"))

#' Coerce object of class \code{classify} to a Data Frame
#' 
#' @param x an object of class \code{\linkS4class{classify}}
#' 
#' @rdname classify-as.data.frame
#' @aliases as.data.frame,classify,missing,missing-method
#' @name classify.as.data.frame
#' @export
#' @importFrom methods setMethod
setMethod("as.data.frame", signature(x = "classify", row.names = "missing", optional = "missing"),
          function(x) {
            data.frame(class = x@.Data, fixation_ids = x@fixation_ids, saccade_ids = x@saccade_ids)
          }
)

# @importFrom methods setGeneric
#setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' Plot classify
#' 
#' Creates a scatter plot of the scanpath between each fixation. Fixation durations are indicated by the size of the fixation point.
#' 
#' @param x an object of class \code{\linkS4class{classify}}
#' @param y an object of class \code{\linkS4class{pva}}
#' 
#' @docType methods
#' @importFrom plyr ddply .
#' @importFrom ggplot2 ggplot geom_path geom_point coord_equal aes labs theme element_blank scale_size_continuous scale_y_reverse scale_y_continuous
#' @importFrom methods setMethod
#' @rdname classify-plot
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

#' Get Fixations
#' 
#' Extracts the coordinates of fixations and their durations from \code{\linkS4class{classify}} 
#' and \code{\linkS4class{pva}} objects
#' 
#' @param class an object of class \code{\linkS4class{classify}}
#' @param dpva an object of class \code{\linkS4class{pva}}
#'
#' @importFrom plyr ddply .
#' @rdname classify-getFixations
#' @aliases getFixations,classify,pva-method
#' @export
#' @importFrom methods setGeneric
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' d.f <- getFixations(d.c, d.pva)
#' d.f
#'  
setGeneric("getFixations", function(class, dpva, ...) standardGeneric("getFixations"))

#' @importFrom methods setMethod
setMethod("getFixations", signature(class = "classify", dpva = "pva"), 
          function(class, dpva, drop=T) {
            f <- subset(ddply(cbind(as.data.frame(dpva), as.data.frame(class)), .(fixation_ids),
                              function(d,t) data.frame(x=mean(d$x),
                                                       y=mean(d$y),
                                                       duration=nrow(d)*t),
                              t=1/dpva@samplerate), fixation_ids!=0)
            rownames(f) <- f$fixation_ids
            d <- f[,c("x","y","duration")]
            if (drop) {
              d <- subset(d, x!=0 & y!=0)
              rownames(d) <- NULL
            }
            d
          })