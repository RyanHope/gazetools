#' Class "mould"
#'
#' A class to hold mould threshold information
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}}{a threshold value}
#'    \item{\code{thresholds}}{vector of class \code{"numeric"}}
#'    \item{\code{resp1}}{vector of class \code{"numeric"}}
#'    \item{\code{resp2}}{vector of class \code{"numeric"}}
#'    \item{\code{gap}}{vector of class \code{"numeric"}}
#'  }
#'
#' @importFrom methods setClass
#' 
#' @docType class
#' @name mould-class
#' @rdname mould-class
#' 
#' @export
#' 
setClass("mould", 
         representation(thresholds = "numeric",
                        resp1 = "numeric",
                        resp2 = "numeric",
                        gap = "numeric"), 
         contains = "numeric")