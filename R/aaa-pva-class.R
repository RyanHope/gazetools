#' Class "pva"
#'
#' A class to hold position, velocity and acceleration of raw gaze data
#'
#' @slot .Data object of class \code{"data.table"}, containing the raw gaze data as well as instantaneous velocity and acceleration values
#' @slot rx the x resolution of the monitor (pixels)
#' @slot ry the y resolution of the monitor (pixels)
#' @slot sw the physical screen width (mm)
#' @slot sh the physical screen height (mm)
#' @slot samplerate the samplerate of the eyetracker
#' @slot sgolayfilt parameters for the savitzky-Golay smoothing filter
#'
#' @importFrom methods setClass
#' @importClassesFrom data.table data.table
#' 
#' @export
#' @docType class
#' 
setClass("pva", 
         representation(rx = "numeric", ry = "numeric",
                        sw = "numeric", sh = "numeric",
                        blinks = "logical",
                        samplerate = "numeric",
                        sgolayfilt = "numeric"),
         contains="data.table")