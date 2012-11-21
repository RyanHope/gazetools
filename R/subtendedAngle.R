subtendedAngle <- function(x1, y1, x2, y2, rx, ry, sw, sh, ez=0, ex=0, ey=0) {
  d1 <- distance2point(x1, y1, rx, ry, sw, sh, ez, ex, ey)
  d2 <- distance2point(x2, y2, rx, ry, sw, sh, ez, ex, ey)
  dX <- sw * (x2 - x1) / rx
  dY <- sh * (y2 - y1) / ry
  dS <- sqrt(dX**2 + dY**2)
  w1 <- d1**2 + d2**2 - dS**2
  w2 <- 2 * d1 * d2
  acos(w1 / w2) * 180 / pi
}