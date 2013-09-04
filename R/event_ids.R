#' Event IDs
#'
#' Assigns unique IDs to runs of a target events in a vector of data
#' 
#' @param x a vector of data
#' @param event the event to id
#'
#' @importFrom Rmisc rsi
#'
#' @export
#' 
#' @example example/event_ids.R
#' 
event_ids <- function(x, event) {
  if (is.character(x)) {
    x <- factor(x)
    event <- which(levels(x) == event)
    x <- as.numeric(x)
  }
  idx <- rsi(x)
  len <- rle(x)$lengths
  val <- rle(x)$values
  fixations <- which(val == event)
  fix <- 1
  ids <- rep(0, length(x))
  for (f in fixations) {
    range <- idx[f]:(idx[f] + len[f] - 1)
    ids[range] <- fix
    fix <- fix + 1
  }
  ids
}