pva <- function(x, y, time, samplerate, rx, ry, sw, sh, ez, ex=0, ey=0,
                order=2, window=11, smooth_position=T)
{
  if (smooth_position) {
    x <- sgolayfilt(x, n=window, p=order, m=0, ts=1/samplerate)
    y <- sgolayfilt(y, n=window, p=order, m=0, ts=1/samplerate)
  }
  
  cx <- rx / 2
  cy <- ry /2
  
  xa <- subtendedAngle(x, cy, cx, cy, rx, ry, sw, sh, ez, ex, ey)
  ya <- subtendedAngle(cx, y, cx, cy, rx, ry, sw, sh, ez, ex, ey)
  
  vx <- sgolayfilt(xa, n=window, p=order, m=1, ts=1/samplerate)
  vy <- sgolayfilt(ya, n=window, p=order, m=1, ts=1/samplerate)
  v <- sqrt(vx**2 + vy**2)
  
  ax <- sgolayfilt(xa, n=window, p=order, m=2, ts=1/samplerate)
  ay <- sgolayfilt(ya, n=window, p=order, m=2, ts=1/samplerate)
  a <- sqrt(ax**2 + ay**2)
  
  d <- data.frame(time=time, x=x, xa=xa, y=y, ya=ya, v=v, a=a)
  attr(d, "sgolayfilt") <- c(order, window)
  d
}