#' Gaze Data Classification (velocity-iterative)
#'
#' Classifies gaze data into saccades and fixations using an iteratively determined velocity threshold.
#' 
#' @template v
#' @param sigma the number of standard deviations between fixation and saccade velocities
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
#' d.c <- classify.VI(d.pva@@v)
#' str(d.c)
#' 
#' # Classification accounting for blinks
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl, pupil=smi_dxl))
#' d.c <- classify.VI(d.pva@@v, blinks=d.pva@@blinks)
#' str(d.c)
#' 
classify.VI <- function(v, vt = 100, sigma = 6, blinks = NULL)
{
  cont = T
  while (cont) {
    f <- v[which(v<vt)]
    vtn <- mean(f) + sigma * sd(f)
    if (abs(vtn-vt)<1)
      cont <- F
    vt <- vtn
  }
  class <- classify.V(v, round(vt, 2), blinks)
  class@algorithm <- "velocity-iterative"
  class
}