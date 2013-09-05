GeomEllipse <- proto(ggplot2:::GeomPath, {
  objname <- "ellipse"
  
  default_stat <- function(.) StatEllipse
  
})

geom_ellipse <- function(mapping = NULL, data = NULL, stat = "ellipse", position = "identity", show_guide = FALSE, ...) {
  GeomEllipse$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}