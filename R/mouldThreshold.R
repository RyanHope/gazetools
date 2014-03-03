#' Mould Threshold
#' 
#' Uses the Mould algorithm to determine the optimal velocity or acceleration threshold for
#' classifying raw gaze data.
#' 
#' @param v a vector of instantaneous velocity values
#' @param samplerate eyetracker samplerate
#' 
#' @return an object of class \code{\linkS4class{mould}}
#' 
#' @export
#' 
#' @references Mould, M. S., Foster, D. H., Amano, K., & Oakley, J. P. (2012). A simple nonparametric method for classifying eye fixations. Vision research, 57, 18-25.
#' 
#' @examples
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                       smi_ezl, smi_exl, smi_eyl))
#' d.mt <- mouldThreshold(d.pva@@v, d.pva@@samplerate)
#' str(d.mt)
#'
mouldThreshold <- function(v, samplerate) {
  peaks <- v[local_maxima(v)]
  r <- range(peaks)
  thresholds <- seq(r[1], r[2], length.out = samplerate)
  l <- length(thresholds)
  resp1 <- sapply(thresholds, function(x) {length(which(peaks > x))})
  resp2 <- sapply(thresholds, function(x) {length(peaks) * (1 - x / l)})
  gap <- resp2 - resp1
  f <- .001
  gap <- lowess(gap, f = f)$y
  while (length(local_maxima(gap)) > 1) {
    f <- f + .001
    gap <- lowess(resp2 - resp1, f = f)$y
  }
  optimal <- thresholds[local_maxima(gap)]
  new("mould", round(optimal, 2), thresholds = thresholds, resp1 = resp1, resp2 = resp2, gap = gap)
}