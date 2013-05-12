#' Gaze Data Classification (velocity-acceleration)
#'
#' Classifies gaze data into saccades and fixations using both velocity and acceleration thresholds.
#' 
#' @template v
#' @template a
#'
#' @export
classify.VA <- function(v, a, vt=30, at=8000)
{
  m <- length(v)
  class <- rep("FIXATION", m)
  vranges <- find_peak_ranges(v, vt)
  apeaks <- find_peaks(a, at)
  for (i in 1:nrow(vranges)) {
    r <- vranges[i,1]:vranges[i,2]
    if (min(r)!=1)
      r <- c(min(r)-1,r)
    if (max(r)!=m)
      r <- c(r,max(r)+1)
    sac <- FALSE
    for (peak in apeaks) {
      if (peak>=min(r) && peak<=max(r)) {
        sac <- TRUE
        break
      }
    }
    if (sac)
      class[r] <- "SACCADE"
  }
  new("classify", class,
      fixation_ids = event_ids(class, "FIXATION"),
      saccade_ids = event_ids(class, "SACCADE"),
      algorithm = "velocity-acceleration", thresholds = c(vt, at))
}