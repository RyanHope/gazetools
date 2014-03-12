#' @export
as.data.frame.pva <- function(x, row.names=NULL, optional=FALSE, ...) {
  data.frame(time = x@time, x = x@sx, y = x@sy, v = x@v, a = x@a)
}

#' Force a 'pva' Object to Belong to a Class
#'
#' @name as
#' @family pva
#' @export
#
setAs("pva","data.frame", function(from) as.data.frame.pva(from))