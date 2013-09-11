#' Local Maxima
#'
#' Finds local maxima in a vector.
#' 
#' @param x a vector of data
#' 
#' @return a vector of local maxima indices
#'
#' @export
#' 
#' @examples
#' local_maxima(c(1,2,3,2,3,4,3,2,1,2,3,2,1))
#' 
local_maxima <- function(x) which(diff(sign(diff(x)))==-2)+1