distance2point <- function(x, y, rx, ry, sw, sh, ez, ex=0, ey=0) {
  dx <- x / rx * sw - sw / 2 + ex
  dy <- y / ry * sh - sh / 2 - ey
  sqrt(ez**2 + dx**2 + dy**2)
}