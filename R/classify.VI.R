classify.VI <- function(v, sigma=6, vpt=100)
{
  cont = T
  while (cont) {
    f <- v[which(v<vpt)]
    vptn <- mean(f) + sigma * sd(f)
    if (abs(vptn-vpt)<1)
      cont <- F
    vpt <- vptn
  }
  class <- classify.V(v, round(vpt, 2))
  slot(class, "algorithm") <- "velocity-iterative"
  class
}