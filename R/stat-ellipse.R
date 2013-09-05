require(proto)

StatEllipse <- proto(ggplot2:::Stat, {
  
  objname <- "ellipse"
  
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPath
  
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales,...)
  }
  
  calculate <- function(., data, scales, level = 0.75, segments = 51,...){
    bceFun(data, level, segments)   
  }
  
})

stat_ellipse <- function(mapping=NULL, data=NULL, geom="path", position="identity", ...) {
  StatEllipse$new(mapping=mapping, data=data, geom=geom, position=position, ...)
}