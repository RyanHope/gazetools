classify.VI <- function(x, y, time, samplerate, rx, ry, sw, sh, ez, ex=0, ey=0,
                        window=11, order=2, smooth_position=T, sigma=6, vpt=100)
{
  d <- as.data.frame(pva(x, y, time, samplerate, rx, ry, sw, sh, ez, ex, ey, order, window, smooth_position))
  d$class <- 0
  
  cont = T
  while (cont) {
    f <- d$v[which(d$v<vpt)]
    vptn <- mean(f) + sigma * sd(f)
    if (abs(vptn-vpt)<1)
      cont <- F
    vpt <- vptn
  }
  vpt <- round(vpt, 2)
  
  m <- length(d$time)
  vranges <- find_peak_ranges(d$v, vpt)
  vpeaks <- find_peaks(d$v, vpt)
  for (i in 1:nrow(vranges)) {
    r <- vranges[i,1]:vranges[i,2]
    if (min(r)!=1)
      r <- c(min(r)-1,r)
    if (max(r)!=m)
      r <- c(r,max(r)+1)
    d$class[r] <- 1
  }
    
  attr(d, "algorithm") <- "velocity-iterative"
  attr(d, "thresholds") <- c(vpt)
  d
}