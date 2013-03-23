setClass("classify", 
         representation(fixation_ids = "numeric", saccade_ids = "numeric",
                        algorithm="character", thresholds="numeric"),
         contains="character")

setMethod("as.data.frame", signature(x = "classify", row.names = "missing", optional = "missing"),
          function(x) {
            data.frame(class = x@.Data, fixation_ids = x@fixation_ids, saccade_ids = x@saccade_ids)
          }
)

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