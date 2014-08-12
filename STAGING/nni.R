#' @importFrom proto proto
#' @importFrom spatstat nnwhich.default
StatKnn <- proto(ggplot2:::Stat, {
  
  objname <- "knn"
  
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomSegment
  
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales,...)
  }
  
  calculate <- function(., data, scales, k=1, ...){
    data <- cbind(data[,c("x","y")], data[nnwhich.default(data, k=k),c("x","y")])
    colnames(data) <- c("x","y","xend","yend")
    data
  }
  
})

#' K-nearest neighbor stat for ggplot2
#' 
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "knn")}
#' 
#' @inheritParams ggplot2:::geom_point
#' @param k Integer. The algorithm will compute the distance to the k-th nearest neighbor
#' 
#' @export
stat_knn <- function(mapping=NULL, data=NULL, geom="segment", position="identity", k=1, ...) {
  StatKnn$new(mapping=mapping, data=data, geom=geom, position=position, k=k, ...)
}

#' @importFrom proto proto
GeomKnn <- proto(ggplot2:::GeomSegment, {
  objname <- "knn"
  
  default_stat <- function(.) StatKnn
  required_aes <- c("x", "y")
})

#' K-nearest neighbor geom for ggplot2
#' 
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "knn")}
#' 
#' @inheritParams ggplot2:::geom_point
#' 
#' @export
geom_knn <- function(mapping = NULL, data = NULL, stat = "knn", position = "identity", show_guide = FALSE, ...) {
  GeomKnn$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}