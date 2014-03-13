#' Gaze Data Classification (velocity)
#'
#' Classifies gaze data into saccades and fixations using a velocity threshold.
#' 
#' @template v
#' @template blinks
#' 
#' @return an object of class \code{\link[=classify-class]{classify}}
#'
#' @export
#' 
#' @family classify
#'  
#' @examples
#' # Classification ignorning blinks
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.V(d.pva@@v)
#' str(d.c)
#' 
#' # Classification accounting for blinks
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl, pupil=smi_dxl))
#' d.c <- classify.V(d.pva@@v, blinks=d.pva@@blinks)
#' str(d.c)
#' 
classify.V <- function(v, vt = 75, blinks = NULL)
{
  m <- length(v)
  class <- rep("FIXATION", m)
  vranges <- find_peak_ranges(v, vt)
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
  if (!is.null(blinks)) {
    class[blinks] <- "BLINK"
    fixation_ids[blinks] <- 0
    saccade_ids[blinks] <- 0
  }
  new("classify", class,
      fixation_ids = fixation_ids,
      saccade_ids = saccade_ids,
      glissade_ids = rep(0, m),
      algorithm = "velocity", thresholds = list(vt=vt))
}