#' Find Peak Ranges Above Threshold
#'
#' Given a vector of data, find ranges of data that exceede a set threshold
#' 
#' @template p
#'
#' @export
#' 
#' @examples
#' find_peak_ranges(c(1,2,3,4,5,4,3,2,1,1,1,2,3,4,3,2,1),3)
#' 
find_peak_ranges <- function(x, threshold)
{
  t <- which(x >= threshold)
  if (length(t) > 0) {
    d <- diff(t)
    n <- which(d > mean(d))  
    data.frame(begin = c(t[1], t[n + 1]), end = c(t[n], tail(t, 1)))
  } 
}