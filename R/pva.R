require(signal)

setClass("pva", 
         representation(time = "numeric", ez = "numeric",
                        ex = "numeric", ey = "numeric",
                        x = "numeric", y = "numeric",
                        sx = "numeric", sy = "numeric",
                        xa = "numeric", ya = "numeric",
                        v = "numeric", a = "numeric",
                        sgolayfilt = "numeric"))

setMethod("as.data.frame", signature(x = "pva", row.names = "missing", optional = "missing"),
          function(x) {
            data.frame(time = x@time, ez = x@ez, ex = x@ex, ey = x@ey, x = x@x, y = x@y,
                       sx = x@sx, sy = x@sy, xa = x@xa, ya = x@ya, v = x@v, a = x@a)
          }
)

setMethod("plot", signature(x = "pva", y = "classify"),
          function(x, y) {
            d <- as.data.frame(x)
            if (class(y)=="classify") {
              d$class <- factor(y)
              d <- melt(d, id=c("time","class"))
            } else
              d <- melt(d, id=c("time"))
            d <- subset(d, variable=="sx" | variable=="sy" | variable=="v" | variable=="a")
            d$variable <- factor(d$variable,labels=c("Gaze X", "Gaze Y", "Velocity", "Acceleration"))
            if (class(y)=="classify") {
              thresholds <- data.frame(variable=levels(d$variable))
              if (length(y@thresholds) > 1) 
                thresholds$intercept <- c(NA,NA,y@thresholds)
              else
                thresholds$intercept <- c(NA,NA,y@thresholds,NA)
              p <- ggplot(d) + geom_point(aes(x=time, y=value, color=class)) + 
                geom_hline(data=thresholds,aes(yintercept=intercept)) +
                scale_color_manual(values=c("black", "red"))
            } else
              p <- ggplot(d) + geom_point(aes(x=time, y=value))
            p + facet_grid(variable~., scales="free_y") + ylab("") + xlab("Time (s)")
          }
)

pva <- function(x, y, time, samplerate, rx, ry, sw, sh, ez,
                    ex = 0, ey = 0, order = 2, window = 11)
{
  if (length(time) < window) return(NULL)
  
  ts <- 1 / samplerate
  
  sx <- sgolayfilt(x, n = window, p = order, m = 0, ts = ts)
  sy <- sgolayfilt(y, n = window, p = order, m = 0, ts = ts)
  
  cx <- rx / 2
  cy <- ry /2
  
  xa <- subtendedAngle(x, cy, cx, cy, rx, ry, sw, sh, ez, ex, ey)
  ya <- subtendedAngle(cx, y, cx, cy, rx, ry, sw, sh, ez, ex, ey)
  
  vx <- sgolayfilt(xa, n = window, p = order, m = 1, ts = ts)
  vy <- sgolayfilt(ya, n = window, p = order, m = 1, ts = ts)
  v <- sqrt(vx**2 + vy**2)
  
  ax <- sgolayfilt(xa, n = window, p = order, m = 2, ts = ts)
  ay <- sgolayfilt(ya, n = window, p = order, m = 2, ts = ts)
  a <- sqrt(ax**2 + ay**2)
  
  new("pva", time = time, ez = ez, ex = ex, ey = ey, x = x, y = y, sx = sx, xa = xa, 
      sy = sy, ya = ya,  v = v, a = a, sgolayfilt = c(order, window))
}