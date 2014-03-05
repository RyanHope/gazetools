#' Class "pva"
#'
#' A class to hold position, velocity and acceleration of raw gaze data
#'
#' @slot time vector of class \code{"numeric"}, containing the time corresponding to raw gaze samples
#' @slot ez vector of class \code{"numeric"}, containing the perpendicular distance from the viewer to the screen (mm)
#' @slot ex vector of class \code{"numeric"}, containing the horizontal offset of the viewer from screen center (mm)
#' @slot ey vector of class \code{"numeric"}, containing the vertical offset of the viewer from screen center (mm)
#' @slot x vector of class \code{"numeric"}, containing the x coordinate of a point on a screen (pixels)
#' @slot y vector of class \code{"numeric"}, containing the y coordinate of a point on a screen (pixels)
#' @slot sx vector of class \code{"numeric"}, containing the smoothed x coordinate of a point on a screen (pixels)
#' @slot sy vector of class \code{"numeric"}, containing the smoothed y coordinate of a point on a screen (pixels)
#' @slot xa vector of class \code{"numeric"}, containing the x coordinate of a point in visual angle relative to the center of screen (degrees)
#' @slot ya vector of class \code{"numeric"}, containing the y coordinate of a point in visual angle relative to the center of screen (degrees)
#' @slot v vector of class \code{"numeric"}, containing the instantaneous velocity (degrees/s)
#' @slot a vector of class \code{"numeric"}, containing the instantaneous acceleration (degrees/s^2)
#' @slot blinks vector of class \code{"logical"}, TRUE if samples belong to a blink
#' @slot rx the x resolution of the monitor (pixels)
#' @slot ry the y resolution of the monitor (pixels)
#' @slot samplerate the samplerate of the eyetracker
#' @slot sgolayfilt parameters for the avitzky-Golay smoothing filter
#'
#' @importFrom methods setClass
#' 
#' @export
#' @docType class
#' 
setClass("pva", 
         representation(time = "numeric", ez = "numeric",
                        ex = "numeric", ey = "numeric",
                        x = "numeric", y = "numeric",
                        sx = "numeric", sy = "numeric",
                        xa = "numeric", ya = "numeric",
                        v = "numeric", a = "numeric",
                        rx = "numeric", ry = "numeric",
                        blinks = "logical",
                        samplerate = "numeric",
                        sgolayfilt = "numeric"))