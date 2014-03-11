\docType{class}
\name{pva-class}
\alias{pva-class}
\title{Class "pva"}
\description{
A class to hold position, velocity and acceleration of raw gaze data
}
\section{Slots}{

\describe{
\item{\code{time}}{vector of class \code{"numeric"}, containing the time corresponding to raw gaze samples}

\item{\code{ez}}{vector of class \code{"numeric"}, containing the perpendicular distance from the viewer to the screen (mm)}

\item{\code{ex}}{vector of class \code{"numeric"}, containing the horizontal offset of the viewer from screen center (mm)}

\item{\code{ey}}{vector of class \code{"numeric"}, containing the vertical offset of the viewer from screen center (mm)}

\item{\code{x}}{vector of class \code{"numeric"}, containing the x coordinate of a point on a screen (pixels)}

\item{\code{y}}{vector of class \code{"numeric"}, containing the y coordinate of a point on a screen (pixels)}

\item{\code{sx}}{vector of class \code{"numeric"}, containing the smoothed x coordinate of a point on a screen (pixels)}

\item{\code{sy}}{vector of class \code{"numeric"}, containing the smoothed y coordinate of a point on a screen (pixels)}

\item{\code{xa}}{vector of class \code{"numeric"}, containing the x coordinate of a point in visual angle relative to the center of screen (degrees)}

\item{\code{ya}}{vector of class \code{"numeric"}, containing the y coordinate of a point in visual angle relative to the center of screen (degrees)}

\item{\code{v}}{vector of class \code{"numeric"}, containing the instantaneous velocity (degrees/s)}

\item{\code{a}}{vector of class \code{"numeric"}, containing the instantaneous acceleration (degrees/s^2)}

\item{\code{blinks}}{vector of class \code{"logical"}, TRUE if samples belong to a blink}

\item{\code{rx}}{the x resolution of the monitor (pixels)}

\item{\code{ry}}{the y resolution of the monitor (pixels)}

\item{\code{sw}}{the physical screen width (mm)}

\item{\code{sh}}{the physical screen height (mm)}

\item{\code{samplerate}}{the samplerate of the eyetracker}

\item{\code{sgolayfilt}}{parameters for the avitzky-Golay smoothing filter}
}}

