

#' Coverage
#'
#' Determines the proportion of fixated ROIs given a scanpath
#' 
#' @param sp an object of class \code{linkS4class{Scanpath}}
#'
#' @return an object of class \code{\linkS4class{Coverage}}
#'
#' @export
#'
coverage <- function(sp) {
  new("Coverage",length(sp@.Data)/length(sp@ROIs), Scanpath=sp)
}

#' Fortify Coverage
#'
#' Method to convert a Coverage object into a data frame useful for plotting.
#' 
#' @param coverage a Coverage object
#'
#' @return a data frame
#' 
#' @export
#' 
fortify.Coverage <- function(coverage) {
  df <- ldply(model@Scanpath@ROIs, function(model) cbind(model@ID,as.data.frame(model@coords)))
  colnames(df) <- c("id","x","y")
  df$covered <- df$id %in% model@Scanpath@.Data
  df
}