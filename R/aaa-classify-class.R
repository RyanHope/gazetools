#' Class "classify"
#'
#' A class to hold gaze data classifications
#'
#' @slot .Data object of class \code{"data.table"}, containing the gaze sample classification information
#' @slot algorithm the algorithm used to classify the gaze data
#' @slot thresholds the threshold settings for the classification algorithm
#' 
#' @importFrom methods setClass
#' @importClassesFrom data.table data.table
#'
#' @export
#' @docType class
#' 
setClass("classify", 
         representation(algorithm="character",
                        thresholds="list"),
         contains="data.table")