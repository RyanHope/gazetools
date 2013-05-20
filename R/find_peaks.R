#' Find Peak Ranges
#'
#' Given a vector of data, find peaks in ranges of data that exceede a set threshold
#' 
#' @template p
#'
#' @export
#' 
#' @example example/find_peaks.R
#' 
find_peaks <- function(x, threshold)
{
  ranges <- find_peak_ranges(x, threshold)
  peaks <- NULL
  if (!is.null(ranges)) {
    for (i in 1:nrow(ranges)) {
      rnge <- ranges[i, 1]:ranges[i, 2]
      r <- x[rnge]
      peaks <- c(peaks, rnge[which(r == max(r))])
    }
  }
  peaks
}