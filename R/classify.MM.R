#' Gaze Data Classification (mixture model)
#'
#' Uses a mixture model to classify gaze data into saccades and fixations.
#' 
#' @param v a vector of instantaneous velocity values
#' @template blinks
#' 
#' @return an object of class \code{\link[=classify-class]{classify}}
#'
#' @autoImports
#' @export
#' 
#' @family classify
#' 
#' @references Ingmar Visser, Maarten Speekenbrink (2010). depmixS4: An R Package for Hidden Markov Models. Journal of Statistical Software, 36(7), 1-21.
#'
#' @example examples/classify.MM.R
#' 
classify.MM <- function(v, blinks = NULL)
{
  d <- data.frame(v=v)
  m <- fit(mix(v~1,nstates=2,data=d))
  c1 <- m@response[[1]][[1]]@parameters$coefficient
  c2 <- m@response[[2]][[1]]@parameters$coefficient
  if (c1 < c2)
    class <- ifelse(m@posterior$state==1,"FIXATION","SACCADE")
  else
    class <- ifelse(m@posterior$state==2,"FIXATION","SACCADE")
  fixation_ids <- event_ids(class, "FIXATION")
  saccade_ids <- event_ids(class, "SACCADE")
  if (!is.null(blinks))
    class[blinks] <- "BLINK"
  new("classify", class,
      fixation_ids = fixation_ids,
      saccade_ids = saccade_ids,
      algorithm = "mixture-model", thresholds = mean(c(max(v[which(class=="FIXATION")]),min(v[which(class=="SACCADE")]))))
}