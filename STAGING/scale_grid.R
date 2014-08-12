#' Grid Scales for ggplot2
#' 
#' Discrete grid scales
#' 
#' @param xintercept a vector of x intercepts
#' @param yintercept a vector of y intercepts
#' @param outer boolean, if TRUE xintercept and yintercept are assumed to contain intercepts for the plot limits
#'
#' @import ggplot2
#' @export
scale_grid <- function(xintercept, yintercept, outer=T) {
  xoffset <- diff(xintercept)[1]/2
  yoffset <- diff(yintercept)[1]/2
  if (outer) {
    xbreaks <- head(xintercept,-1) + xoffset
    ybreaks <- head(yintercept,-1) + yoffset
    ncol <- length(xintercept) - 1
    nrow <- length(yintercept) - 1
  } else {
    xbreaks <- xintercept - xoffset
    ybreaks <- yintercept - xoffset
    ncol <- length(xintercept) + 1
    nrow <- length(yintercept) + 1
  }
  list(scale_x_continuous(breaks=xbreaks,labels=letters[1:ncol]),
       scale_y_continuous(breaks=ybreaks,labels=1:nrow))
}