setClass("mould", 
         representation(thresholds="numeric", resp1="numeric", resp2="numeric", gap="numeric"), 
         contains="numeric")

setMethod("plot", signature(x="mould", y="missing"),
          function(x,y,...) {
            optimal<-as.numeric(x)
            dd <- data.frame(resp1=slot(x,"resp1"),
                             resp2=slot(x,"resp2"),
                             gap=slot(x,"gap"),
                             thresholds=slot(x,"thresholds"))
            ggplot(dd, aes(x=thresholds, y=resp1)) + geom_area(fill="gray") +
              ylab("Frequency of local speed maxima exceeding threshold") +
              xlab("Speed threshold, deg/s") + 
              ylim(0,max(dd$resp1)) +
              geom_line(aes(x=thresholds,y=resp2),linetype="dashed") +
              geom_line(aes(x=thresholds,y=gap)) + 
              geom_vline(xintercept=optimal,size=1.5) +
              geom_text(data=data.frame(x=optimal, y=mean(dd$resp2)), aes(x=x, y=y, hjust=-.15), label=as.character(sprintf("%.2f",optimal)))
          }
)

mouldThreshold <- function(velocity, samplerate)
{
  peaks <- velocity[maxima(velocity)]
  r <- range(peaks)
  thresholds <- seq(r[1],r[2],length.out=samplerate)
  resp1 <- sapply(thresholds, function(x){length(which(peaks>x))})
  resp2 <- sapply(thresholds, function(x){length(peaks)*(1-x/length(thresholds))})
  gap <- resp2 - resp1
  f <- .001
  gap <- lowess(gap, f=f)$y
  while (length(maxima(gap))>1) {
    f <- f + .001
    gap <- lowess(resp2 - resp1, f=f)$y
  }
  optimal <- thresholds[which(gap==max(gap))]
  new("mould",optimal,thresholds=thresholds,resp1=resp1,resp2=resp2,gap=gap)
}