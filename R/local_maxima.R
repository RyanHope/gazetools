#' Local Maxima
#'
#' Finds indices of local maxima in a vector.
#' 
#' @param x a vector of data
#'
#' @export
local_maxima <- function(x) which(diff(sign(diff(x)))==-2)+1