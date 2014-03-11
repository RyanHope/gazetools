#' Mould Threshold
#' 
#' Uses the Mould algorithm to determine the optimal velocity or acceleration threshold for
#' classifying raw gaze data.
#' 
#' @param v a vector of instantaneous velocity values
#' @param samplerate eyetracker samplerate
#' @param blinks a vector indicating whether each sample belongs to a blink, if non-null blinks will not be included in analysis
#' 
#' @return an object of class \code{mould}
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
mouldThreshold <- function(v, samplerate, blinks=NULL) {
  if (!is.null(blinks) && length(v)==length(blinks))
    v <- v[!blinks]
  peaks <- v[local_maxima(v)]
  r <- range(peaks)
  thresholds <- seq(r[1], r[2], length.out = samplerate)
  l <- length(thresholds)
  resp1 <- sapply(thresholds, function(x) {length(which(peaks > x))})
  resp2 <- sapply(thresholds, function(x) {length(peaks) * (1 - x / l)})
  gap.old <- resp2 - resp1
  f <- .001
  gap.new <- lowess(gap.old, f = f)$y
  m.new <- local_maxima(gap.new)
  m.old <- m.new
  nt.new <- length(m.old)
  nt.old <- nt.new
  while (nt.new > 1) {
    if (nt.new > nt.old)
      break
    f <- f + .001
    gap.old <- gap.new
    gap.new <- lowess(resp2 - resp1, f = f)$y
    nt.old <- nt.new
    m.old <- m.new
    m.new <- local_maxima(gap.new)
    nt.new <- length(m.new)
  }
  if (nt.new !=1) {
    m <- m.old
    gap <- gap.old
  } else {
    m <- m.new
    gap <- gap.new
  }
  optimal <- thresholds[m[which(gap[m]==max(gap[m]))]]
  new("mould", round(optimal, 2), thresholds = thresholds, resp1 = resp1, resp2 = resp2, gap = gap)
}