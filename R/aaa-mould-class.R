#' Class "mould"
#'
#' A class to hold mould threshold information
#'
#' @slot .Data a threshold value
#' @slot thresholds vector of class \code{"numeric"}
#' @slot resp1 vector of class \code{"numeric"}
#' @slot resp2 vector of class \code{"numeric"}
#' @slot gap vector of class \code{"numeric"}
#'
#' @importFrom methods setClass
#' 
#' @export
#' @docType class
#' 
setClass("mould", 
         representation(thresholds = "numeric",
                        resp1 = "numeric",
                        resp2 = "numeric",
                        gap = "numeric"), 
         contains = "numeric")