% Built by roxygen3 0.1
\name{distance_2_point}
\alias{distance_2_point}
\title{Distance to Point}
\usage{
  distance_2_point(x, y, rx, ry, sw, sh, ez, ex = 0,
    ey = 0)
}
\arguments{
  \item{x}{the x coordinate of a point on a screen (pixels)}

  \item{y}{the y coordinate of a point on a screen (pixels)}

  \item{rx}{the x resolution of the monitor (pixels)}

  \item{ry}{the y resolution of the monitor (pixels)}

  \item{sw}{the physical screen width (mm)}

  \item{sh}{the physical screen height (mm)}

  \item{ez}{the perpendicular distance from the viewer to the screen (mm)}

  \item{ex}{the horizontal offset of the viewer from screen center (mm)}

  \item{ey}{the vertical offset of the viewer from screen center (mm)}
}

\description{
Takes an x and y screen coordinate and returns the physical distance
from the observer to that point on the screen.
}
\examples{
distance_2_point(840, 525, 1680, 1050, 473.76, 296.1, 750)
}

