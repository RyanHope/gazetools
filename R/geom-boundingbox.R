GeomBoundingbox <- proto(ggplot2:::GeomRect, {
  objname <- "boundingbox"
  
  default_stat <- function(.) StatBoundingbox
  default_aes <- function(.) aes(colour="black", fill=NA, size=0.5, linetype=1, alpha = NA)
  
})

geom_boundingbox <- function(mapping = NULL, data = NULL, stat = "boundingbox", position = "identity", show_guide = FALSE, ...) {
  GeomBoundingbox$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}