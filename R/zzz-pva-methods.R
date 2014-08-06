#' @export
as.data.frame.pva <- function(x, row.names=NULL, optional=FALSE, ...) {
  d <- data.frame(timestamp = x@timestamp, time = x@time, x = x@x, y = x@y, v = x@v, a = x@a,
                  sx = x@sx, sy = x@sy, xa = x@xa, ya = x@ya,
                  ez = x@ez, ex = x@ex, ey = x@ey, blinks = x@blinks)
  attr(d, "samplerate") <- x@samplerate
  attr(d, "sgolayfilt") <- x@sgolayfilt
  attr(d, "rx") <- x@rx
  attr(d, "ry") <- x@ry
  attr(d, "sw") <- x@sw
  attr(d, "sh") <- x@sh
  d
}

#' Force a 'pva' Object to Belong to a Class
#'
#' @name as
#' @family pva
#' @export
#
setAs("pva","data.frame", function(from) as.data.frame.pva(from))