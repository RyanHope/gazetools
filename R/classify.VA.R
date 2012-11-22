classify.VA <- function(x, y, time, samplerate, rx, ry, sw, sh, ez, ex=0, ey=0,
                             vt=30, at=8000, window=11, order=2, smooth_position=T)
{
  d <- as.data.frame(pva(x, y, time, samplerate, rx, ry, sw, sh, ez, ex, ey, window, order, smooth_position))
  d$class <- 0
  
  m <- length(d$time)
  vranges <- find_peak_ranges(d$v, vt)
  apeaks <- find_peaks(d$a, at)
  for (i in 1:nrow(vranges)) {
    r <- vranges[i,1]:vranges[i,2]
    if (min(r)!=1)
      r <- c(min(r)-1,r)
    if (max(r)!=m)
      r <- c(r,max(r)+1)
    sac <- FALSE
    for (peak in apeaks) {
      if (peak>=min(r) && peak<=max(r)) {
        sac <- TRUE
        break
      }
    }
    if (sac)
      d$class[r] <- 1
  }
  
  attr(d, "algorithm") <- "velocity-acceleration"
  attr(d, "thresholds") <- c(vt,at)
  d
}