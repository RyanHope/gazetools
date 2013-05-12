\name{angle2pixels}
\alias{angle2pixels}
\title{Angle to Pixels}
\usage{
  angle2pixels(alpha, rx, ry, sw, sh, ez, ex = 0, ey = 0,
    x = rx/2, y = ry/2)
}
\arguments{
  \item{alpha}{the target visual angle}

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

  \item{x}{the x coordinate of a point on a screen
  (pixels)}

  \item{y}{the y coordinate of a point on a screen
  (pixels)}
}
\description{
  Returns a distance (in pixels) given a visual angle and
  distance.
}

