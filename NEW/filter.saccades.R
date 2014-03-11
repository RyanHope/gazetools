#' @export
filter.saccades <- function(dpva, class, angle=1)
{
  d <- data.frame(x=dpva@x, y=dpva@y, ez=dpva@ez, ex=dpva@ex, ey=dpva@ey, saccade_id=class@saccade_ids)
  ddply(subset(d, saccade_id!=0), .(saccade_id), function(x, rx, ry, sw, sh) {
    #print(x)
    data.frame(x1=head(x$x,1), y1=head(x$y,1), x2=tail(x$x,1), y2=tail(x$y,1),
    a=subtended_angle(head(x$x,1), head(x$y,1), tail(x$x,1), tail(x$y,1), rx, ry, sw, sh, mean(x$ez), mean(x$ex), mean(x$ey)))
  }, dpva@rx, dpva@ry, dpva@sw, dpva@sh)
}