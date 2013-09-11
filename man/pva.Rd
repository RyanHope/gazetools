\name{pva}
\alias{pva}
\title{Position, Velocity, Acceleration}
\usage{
  pva(x, y, samplerate, rx, ry, sw, sh, ez, ex = 0, ey = 0,
    order = 2, window = 11, pupil = NULL)
}
\arguments{
  \item{x}{the x coordinate of a point on a screen
  (pixels)}

  \item{y}{the y coordinate of a point on a screen
  (pixels)}

  \item{samplerate}{the samplerate of the eyetracker}

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

  \item{order}{the Savitzky-Golay filter order}

  \item{window}{the Savitzky-Golay filter window length
  (must be odd)}

  \item{pupil}{a vector of class \code{numeric}; the
  vertical diameter of the pupil}
}
\value{
  an object of class \code{\linkS4class{pva}}
}
\description{
  Takes standard eye tracker gaze information and computes
  smoothed position, velocity and acceleration profiles
  using a Savitzky-Golay filter.
}
\examples{
data(smi)
d.pva <- with(smi, pva(smi_sxl, smi_syl,
                       500, 1680, 1050, 473.76, 296.1,
                       smi_ezl, smi_exl, smi_eyl))
str(d.pva)
}

