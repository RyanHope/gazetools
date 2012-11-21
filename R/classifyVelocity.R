library(signal)

classifyVelocity <- function(x, y, time, samplerate, sx, sy, sw, sh, ez, ex=0, ey=0,
                             vt=30, at=8000, window=11, order=2, smooth_position=T)
{
  if (smooth_position) {
    x <- sgolayfilt(x, n=window, p=order, m=0, ts=1/samplerate)
    y <- sgolayfilt(y, n=window, p=order, m=0, ts=1/samplerate)
  }
  
  cx <- sx / 2
  cy <- sy /2
  
  xa = subtendedAngle(x, cy, cx, cy, sx, sy, sw, sh, ez, ex, ey)
  ya = subtendedAngle(cx, y, cx, cy, sx, sy, sw, sh, ez, ex, ey)
  
  vx <- sgolayfilt(xa, n=window, p=order, m=1, ts=1/samplerate)
  vy <- sgolayfilt(ya, n=window, p=order, m=1, ts=1/samplerate)
  v <- sqrt(vx**2 + vy**2)
  
  ax <- sgolayfilt(xa, n=window, p=order, m=2, ts=1/samplerate)
  ay <- sgolayfilt(ya, n=window, p=order, m=2, ts=1/samplerate)
  a <- sqrt(ax**2 + ay**2)
  
  data.frame(time=time, x=x, y=y, v=v, a=a)
}