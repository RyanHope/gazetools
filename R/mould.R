setClass("mould", 
         representation(thresholds = "numeric", resp1 = "numeric", resp2 = "numeric", gap = "numeric"), 
         contains = "numeric")

setMethod("plot", signature(x = "mould", y = "missing"),
          function(x, y, ...) {
            optimal<-as.numeric(x)
            dd <- data.frame(resp1 = slot(x, "resp1"),
                             resp2 = slot(x, "resp2"),
                             gap = slot(x, "gap"),
                             thresholds = slot(x, "thresholds"))
            ggplot(dd, aes(x = thresholds, y = resp1)) + geom_area(fill = "gray") +
              ylab("Frequency of local speed maxima exceeding threshold") +
              xlab("Speed threshold, deg/s") + 
              ylim(0, max(dd$resp1)) +
              geom_line(aes(x = thresholds, y = resp2), linetype = "dashed") +
              geom_line(aes(x = thresholds, y = gap)) + 
              geom_vline(xintercept = optimal, size = 1.5) +
              geom_text(data = data.frame(x = optimal, y = mean(dd$resp2)),
                        aes(x = x, y = y, hjust = -.15), label = as.character(sprintf("%.2f", optimal)))
          }
)