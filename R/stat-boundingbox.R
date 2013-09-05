require(proto)

StatBoundingbox <- proto(ggplot2:::Stat, {
  
  objname <- "boundingbox"
  
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomRect
  
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales,...)
  }
  
  calculate <- function(., data, scales, ...){
    data.frame(xmin=min(data$x), xmax=max(data$x), ymin=min(data$y), ymax=max(data$y))
  }
  
})

stat_boundingbox <- function(mapping=NULL, data=NULL, geom="path", position="identity", ...) {
  StatBoundingbox$new(mapping=mapping, data=data, geom=geom, position=position, ...)
}