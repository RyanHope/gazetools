\name{subtendedAngle}
\alias{subtendedAngle}
\title{Subtended Angle}
\usage{
  subtendedAngle(x1, y1, x2, y2, rx, ry, sw, sh, ez,
    ex = 0, ey = 0)
}
\arguments{
  \item{x1}{the x coordinate of the first point on a screen
  (pixels)}

  \item{y1}{the y coordinate of the first point on a screen
  (pixels)}

  \item{x2}{the x coordinate of the second point on a
  screen (pixels)}

  \item{y2}{the y coordinate of the second point on a
  screen (pixels)}

  \item{rx}{the x resolution of the monitor (pixels)}

  \item{ry}{the y resolution of the monitor (pixels)}

  \item{sw}{the physical screen width (mm)}

  \item{sh}{the physical screen height (mm)}

  \item{ez}{the perpendicular distance from the viewer to
  the screen (mm)}

  \item{ex}{the horizontal offset of the viewer from screen
  center (mm)}

  \item{ey}{the vertical offset of the viewer from screen
  center (mm)}
}
\description{
  Takes two screen coordinates and returns the angle (in
  degrees) subtended by those two points.
}

