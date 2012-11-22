library(lattice)
library(reshape)
library(latticeExtra)

pvaPlot <- function(d)
{
  title <- paste(attr(d, "algorithm"), "[", paste(attr(d, "thresholds"), collapse=","), "]", sep=" ")
  thresholds <- list(x=NA, y=NA, v=NA, a=NA)
  if (length(attr(d,"thresholds"))>0) {
    thresholds$v <- attr(d,"thresholds")[1]
    if (length(attr(d,"thresholds"))>1)
      thresholds$a <- attr(d,"thresholds")[2]
  }
  d <- melt(d, id=c("time","class"))
  p <- xyplot(value~time|variable, d, layout=c(1,NA), as.table=T, pch=20, cex=.5,
              scales=list(y=list(relation="free")), col=ifelse(d$class>0,hsv(0,1,1),"black"),
              panel=function(x, y, subscripts, ...) {
                panel.xblocks(x, ifelse(d$class==1,hsv(0,.1,1),ifelse(d$class==-1, hsv(240,.1,1), "white")))
                panel.xyplot(x,y,...)
                t <- unique(d[subscripts,]$variable)
                if (!is.na(thresholds[t]))
                  panel.abline(h=thresholds[t])
              }, xlab="Time [ms]", ylab="Gaze parameter", main=title)
  p <- resizePanels(p, h=c(.5,.5,1,1))
  print(p)
}