\name{classify.VI}
\alias{classify.VI}

\title{
  Gaze Data Classification (velocity-iterative)
}

\description{
  Classifies gaze data into saccades and fixations using an iteratively determined velocity threshold.
}

\usage{
  classify.VI(x, y, time, samplerate, rx, ry, sw, sh, ez, ex=0, ey=0, 
              window=11, order=2, smooth_position=T, sigma=6, vpt=100)
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
    If TRUE, the x and y gaze positions returned will be smoothed
  }
  \item{sigma}{
    The number of standard deviations used each iteration step to determine velocity threshold
  }
  \item{vpt}{
    The initial velocity threshold
  }
}

\value{
  A data.frame with 6 columns: time, x, y, v, a, class. The first 5 columns are identical to 
  what pva() returns, the 6th column contains the classification value.
}

\author{
  Ryan M. Hope
}

\examples{
  # Need an example
}