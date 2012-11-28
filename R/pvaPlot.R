library(lattice)
library(reshape)
library(latticeExtra)

pvaPlot <- function(d, layout=c(1,NA), thresholds=NULL)
{
  title <- paste(attr(d, "algorithm"), "[", paste(attr(d, "thresholds"), collapse=","), "]", sep=" ")
  if (is.null(thresholds)) {
    thresholds <- list(x=NA, y=NA, v=NA, a=NA)
    if (length(attr(d,"thresholds"))>0) {
      thresholds$v <- attr(d,"thresholds")[1]
      if (length(attr(d,"thresholds"))>1)
        thresholds$a <- attr(d,"thresholds")[2]
    }
  }
  d <- melt(d, id=c("time","class"))
  p <- xyplot(value~time|variable, d, layout=layout, as.table=T, pch=20, cex=.5,
              scales=list(y=list(relation="free")), col=ifelse(d$class>0,hsv(0,1,1),"black"),
              panel=function(x, y, subscripts, ...) {
                panel.xblocks(x, ifelse(d$class==1,hsv(0,.1,1),ifelse(d$class==-1, hsv(240,.1,1), "white")))
                panel.xyplot(x,y,...)
                t <- as.character(head(d[subscripts,]$variable,1))
                if (t %in% names(thresholds))
                  panel.abline(h=thresholds[[t]])
              }, xlab="Time [ms]", ylab="Gaze parameter", main=title)
  #p <- resizePanels(p, h=c(.5,.5,1,1))
}