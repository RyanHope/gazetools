#' gazetools
#' 
#' A collection of tools for processing and classifying eye gaze data.
#' 
#' @name gazetools
#' @docType package
#' @import methods rgeos maptools deldir sp modeest signal zoo Rmisc reshape plyr spatstat ggplot2
NULL

#' Gaze data from a SMI RED 500 eyetracker at 500 Hz.
#' 
#' \itemize{
#'   \item sample time
#'   \item sample type
#'   \item horizontal gaze location of left eye
#'   \item horizontal gaze location of right eye
#'   \item vertical gaze location of left eye
#'   \item vertical gaze location of right eye
#'   \item width of left pupil
#'   \item width of right pupil
#'   \item height of left pupil
#'   \item height of right pupil
#'   \item horizontal offset of the left eye relative to the center of the screen
#'   \item horizontal offset of the right eye relative to the center of the screen
#'   \item vertical offset of the left eye relative to the center of the screen
#'   \item vertical offset of the right eye relative to the center of the screen
#'   \item distance of the left eye to the center of the screen
#'   \item distance of the right eye to the center of the screen
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name smi
#' @usage data(smi)
#' @format A data frame with 3437 rows and 15 variables
NULL