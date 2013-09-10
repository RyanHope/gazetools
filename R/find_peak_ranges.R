#' Find Peak Ranges Above Threshold
#'
#' Given a vector of data, find ranges of data that exceede a set threshold
#' 
#' @template p
#'
#' @export
#' 
#' @example example/find_peak_ranges.R
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