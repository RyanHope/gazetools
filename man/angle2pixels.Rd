\name{angle2pixels}
\alias{angle2pixels}

\title{
  Angle to Pixels
}

\description{
  Returns a distance (in pixels) given a visual angle and distance.
}

\usage{
  angle2pixels(alpha, rx, ry, sw, sh, ez, ex=0, ey=0, x=rx/2, y=ry/2)
}

\arguments{
  \item{alpha}{
    The target visual angle
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
    Horizontal offset of the viewer from screen center (mm)
  }
  \item{ey}{
    Vertical offset of the viewer from screen center (mm)
  }
  \item{x}{
    The center x coordinate of the target visual angle (pixels)
  }
  \item{y}{
    The center y coordinate of the target visual angle (pixels)
  }
}

\value{
  The diameter of a given visual angle in pixels
}

\author{
  Ryan M. Hope
}

\examples{
  angle2pixels(1, 1680, 1050, 473.76, 296.1, 750)
}