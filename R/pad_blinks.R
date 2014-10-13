pad_blinks <- function(x, pad) {
  N <- length(x)
  out <- rep(FALSE, N)
  for (i in which(x)) out[pmax(i-pad,1):pmin(i+pad,N)] <- TRUE
  out
}
