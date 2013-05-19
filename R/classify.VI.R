#' Gaze Data Classification (velocity-iterative)
#'
#' Classifies gaze data into saccades and fixations using an iteratively determined velocity threshold.
#' 
#' @template v
#' @param sigma the number of standard deviations between fixation and saccade velocities
#' 
#' @return an object of class \code{\linkS4class{classify}}
#'
#' @export
classify.VI <- function(v, vt=100, sigma=6)
{
  cont = T
  while (cont) {
    f <- v[which(v<vt)]
    vtn <- mean(f) + sigma * sd(f)
    if (abs(vtn-vt)<1)
      cont <- F
    vt <- vtn
  }
  class <- classify.V(v, round(vt, 2))
  slot(class, "algorithm") <- "velocity-iterative"
  class
}