#' @export
stat_boundingbox <- function(mapping = NULL, data = NULL, geom = "boundingbox", position = "identity", ...) {
  gazetools:::StatBoundingBox$new(mapping = mapping, data = data, ...)
}

#' @import proto
#' @import ggplot2
#' @importFrom plyr ddply .
StatBoundingBox <- proto(ggplot2:::Stat, {
  objname <- "boundingbox"
  
  default_geom <- function(.) gazetools:::GeomBoundingBox
  
  required_aes <- c("x", "y")
  
  calculate_groups <- function(., data, scales, ...) {
    print(data)
    ddply(data, .(PANEL, group), function(d) 
      data.frame(xmin=min(d$x), xmax=max(d$x), ymin=min(d$y), ymax=max(d$y)))
  }
  
})

#' @export
geom_boundingbox <- function(mapping = NULL, data = NULL, stat = "boundingbox", position = "identity", show_guide = FALSE, ...) {
  gazetools:::GeomBoundingBox$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

#' @import proto
#' @import ggplot2
GeomBoundingBox <- proto(ggplot2:::GeomRect, {
  objname <- "boundingbox"
  
  default_stat <- function(.) gazetools:::StatBoundingBox
  default_aes <- function(.) aes(colour="black", fill=NA, size=0.5, linetype=1, alpha = NA)
  
})