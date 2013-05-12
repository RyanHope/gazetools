#' Gaze Data Classification (velocity)
#'
#' Classifies gaze data into saccades and fixations using a velocity threshold.
#' 
#' @template v
#'
#' @export
classify.V <- function(v, vt = 75)
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
  new("classify", class,
      fixation_ids = event_ids(class, "FIXATION"),
      saccade_ids = event_ids(class, "SACCADE"),
      algorithm = "velocity", thresholds = c(vt))
}