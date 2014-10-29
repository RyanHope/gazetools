vincent <- function(x, ...) {
  .quantiles <- do.call(rbind,lapply(x, quantile, ...))
  .mean <- apply(.quantiles, 2, mean)
  .sd <- apply(.quantiles, 2, sd)
  .probs <- sapply(names(.mean),function(x) as.numeric(sub("%","",x))/100)
  data.table(probs=.probs,mean=.mean,sd=.sd)
}

