\name{pva}
\alias{pva}

\title{
  Position, Velocity, Acceleration
}

\description{
  Takes standard gaze information and computes smoothed position, velocity
  and acceleration profiles using a Savitzky-Golay filter.
}
\usage{
  pva(x, y, time, samplerate, rx, ry, sw, sh, ez, ex=0, ey=0,
                order=2, window=11, smooth_position=T)
}

\arguments{
  \item{x}{
    A vector of x gaze samples (pixels)
  }
  \item{y}{
    A vector of y gaze samples (pixels)
  }
  \item{time}{
    A vector of time samples
  }
  \item{samplerate}{
    The samping rate of the gaze samples
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
  \item{order}{
    The Savitzky-Golay filter order
  }
  \item{window}{
    The Savitzky-Golay filter window length (must be odd)
  }
  \item{smooth_position}{
    If TRUE, the x and y gaze positions returned will be smoothed.
  }
}

\value{
  A data.frame with 5 columns: time, x, y, v, a.
}

\author{
  Ryan M. Hope
}

\examples{
  # Need an example
}