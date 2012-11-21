\name{distance2point}
\alias{distance2point}

\title{
  Distance to Point
}

\description{
  Takes an x and y screen coordinate and returns the physical distance
  from the observer to that point on the screen.
}
\usage{
  distance2point(x, y, rx, ry, sw, sh, ez, ex=0, ey=0)
}

\arguments{
  \item{x}{
    The x coordinate of a point on a screen (pixels)
  }
  \item{y}{
    The y coordinate of point on a screen (pixels)
  }
  \item{rx}{
    The x resolution of the monitor (pixels)
  }
  \item{ry}{
    The y resolution of the monitor (pixels)
  }
  \item{sw}{
    The physical screen width (mm)
  }
  \item{sh}{
    The physical screen height (mm)
  }
  \item{ez}{
    Perpendicular distance from the viewer to the screen (mm)
  }
  \item{ex}{
    Horizontal offset of the viewer above screen center (mm)
  }
  \item{ey}{
    Horizontal offset of the viewer above screen center (mm)
  }
}

\value{
  A distance in milimeters
}

\details{
  The "sw", "sh", "ez", "ex" and "ey" arguments can be any unit as long as they are the same.
}

\author{
  Ryan M. Hope
}

\examples{
  distance2point(840, 525, 1680, 1050, 473.76, 296.1, 750)
}