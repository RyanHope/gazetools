#' Gaze Data Classification (velocity)
#'
#' Classifies gaze data into saccades and fixations using a velocity threshold.
#' 
#' @template v
#' @template blinks
#' 
#' @return an object of class \code{\linkS4class{classify}}
#'
#' @export
#' 
#' @example example/pva.R
#' @example example/classify.V.R
#' @example example/classify.V?-out.R
#' 
#' @family classify
#'  
classify.V <- function(v, vt = 75, blinks = NULL)
{
  m <- length(v)
  class <- rep("FIXATION", m)
  vranges <- find_peak_ranges(v, vt)
  vpeaks <- find_peaks(v, vt)
  for (i in 1:nrow(vranges)) {
    r <- vranges[i,1]:vranges[i,2]
    if (min(r) != 1)
      r <- c(min(r) - 1, r)
    if (max(r) != m)
      r <- c(r, max(r) + 1)
    class[r] <- "SACCADE"
  }
  fixation_ids <- event_ids(class, "FIXATION")
  saccade_ids <- event_ids(class, "SACCADE")
  if (!is.null(blinks))
    class[blinks] <- "BLINK"
  new("classify", class,
      fixation_ids = fixation_ids,
      saccade_ids = saccade_ids,
      algorithm = "velocity", thresholds = c(vt))
}