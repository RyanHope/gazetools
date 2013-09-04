#' Gaze Data Classification (velocity-acceleration)
#'
#' Classifies gaze data into saccades and fixations using both velocity and acceleration thresholds.
#' 
#' @template v
#' @template a
#' @template blinks
#' 
#' @return an object of class \code{\linkS4class{classify}}
#'
#' @export
#' 
#' @family classify
#' 
#' @encoding latin1
#' @references Nyström, M., & Holmqvist, K. (2010). An adaptive algorithm for fixation, saccade, and glissade detection in eyetracking data. Behavior Research Methods, 42(1), 188-204.
#' 
#' @examples
#' # Classification ignorning blinks
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.VA(d.pva@@v, d.pva@@a)
#' str(d.c)
#' 
#' # Classification accounting for blinks
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl, pupil=smi_dxl))
#' d.c <- classify.VA(d.pva@@v, d.pva@@a, blinks=d.pva@@blinks)
#' str(d.c)
#' 
classify.VA <- function(v, a, vt = 30, at = 8000, blinks = NULL)
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
  fixation_ids <- event_ids(class, "FIXATION")
  saccade_ids <- event_ids(class, "SACCADE")
  if (!is.null(blinks))
    class[blinks] <- "BLINK"
  new("classify", class,
      fixation_ids = fixation_ids,
      saccade_ids = saccade_ids,
      algorithm = "velocity-acceleration", thresholds = c(vt, at))
}