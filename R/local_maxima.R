#' Local Maxima
#'
#' Finds local maxima in a vector.
#' 
#' @param x a vector of data
#' 
#' @return a vector of local maxima indices
#'
#' @export
local_maxima <- function(x) which(diff(sign(diff(x)))==-2)+1