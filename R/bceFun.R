#' @import MASS
bceFun <- function(data, level=.9, segments=51) {
  dfn <- 2
  dfd <- length(data$x) - 1
  if (dfd < 3){
    ellipse <- rbind(c(NA,NA))  
  } else {
    v <- cov.trob(cbind(data$x, data$y))
    shape <- v$cov
    center <- v$center
    radius <- sqrt(dfn * qf(level, dfn, dfd))
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- t(center + radius * t(unit.circle %*% chol(shape)))
  }
  ellipse <- as.data.frame(ellipse)
  colnames(ellipse) <- c("x","y")
  ellipse
}