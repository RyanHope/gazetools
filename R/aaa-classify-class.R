#' Class "classify"
#'
#' A class to hold gaze data classifications
#'
#' @slot .Data vector of class \code{"character"}, containing the gaze sample classification
#' @slot fixation_ids vector of class \code{"numeric"}, containing unique fixation ids
#' @slot saccade_ids vector of class \code{"numeric"}, containing unique saccade ids
#' @slot glissade_ids vector of class \code{"numeric"}, containing unique glissade ids
#' @slot algorithm the algorithm used to classify the gaze data
#' @slot thresholds the threshold settings for the classification algorithm
#' 
#' @importFrom methods setClass
#'
#' @export
#' @docType class
#' 
setClass("classify", 
         representation(fixation_ids = "numeric",
                        saccade_ids = "numeric",
                        glissade_ids = "numeric",
                        blink_ids = "numeric",
                        algorithm="character",
                        thresholds="list"),
         contains="character")