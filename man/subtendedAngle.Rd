\name{subtendedAngle}
\alias{subtendedAngle}

\title{
  Subtended Angle
}

\description{
  Takes two screen coordinates and returns the angle (in degrees)
  subtended by those two points.
}

\usage{
  subtendedAngle(x1, y1, x2, y2, viewerDistance = 58.74, viewerHeight = 4.55, resolutionX = 1280, resolutionY = 1024, screenWidth = 33.97, screenHeight = 27.31)
}

\arguments{
  \item{x1}{
    The x coordinate of the first point on a screen (pixels)
  }
  \item{y1}{
    The y coordinate of the first point on a screen (pixels)
  }
  \item{x2}{
    The x coordinate of the second point on a screen (pixels)
  }
  \item{y2}{
    The y coordinate of the second point on a screen (pixels)
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
  The angle (in degrees) subtended by two points
}

\details{
  The "sw", "sh", "ez", "ex" and "ey" arguments can be any unit as long as they are the same.
}

\author{
  Ryan M. Hope
}

\examples{
  subtendedAngle(835, 525, 845, 525, 1680, 1050, 473.76, 296.1, 750)
}