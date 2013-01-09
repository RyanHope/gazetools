mouldVelocityThreshold <- function(d, samplerate)
{
  peaks <- d[maxima(d)]
  r <- range(peaks)
  thresholds <- seq(r[1], r[2], length.out = samplerate)
  resp1 <- sapply(thresholds, function(x) {length(which(peaks > x))})
  resp2 <- sapply(thresholds, function(x) {length(peaks) * (1 - x / length(thresholds))})
  gap <- resp2 - resp1
  f <- .001
  gap <- lowess(gap, f = f)$y
  while (length(maxima(gap)) > 1) {
    f <- f + .001
    gap <- lowess(resp2 - resp1, f = f)$y
  }
  optimal <- thresholds[maxima(gap)]
  new("mould", round(optimal, 2), thresholds = thresholds, resp1 = resp1, resp2 = resp2, gap = gap, type = "velocity")
}

mouldAccelerationThreshold <- function(d, samplerate)
{
  peaks <- d[maxima(d)]
  r <- range(peaks)
  thresholds <- seq(r[1], r[2], length.out = samplerate)
  resp1 <- sapply(thresholds, function(x) {length(which(peaks > x))})
  resp2 <- sapply(thresholds, function(x) {length(peaks) * (1 - x / (length(thresholds)^2))})
  gap <- resp2 - resp1
  f <- .001
  gap <- lowess(gap, f = f)$y
  while (length(maxima(gap)) > 1) {
    f <- f + .001
    gap <- lowess(resp2 - resp1, f = f)$y
  }
  optimal <- thresholds[maxima(gap)]
  new("mould", round(optimal, 2), thresholds = thresholds, resp1 = resp1, resp2 = resp2, gap = gap, type = "acceleration")
}