#' @export
unique_ids <- function(class) {
  class <- as.numeric(factor(class,labels=1:length(unique(factor(class)))))
  start <- rsi(class)
  len <- rle(a)$lengths
  count <- 1
  for (i in 1:length(start)) {
    class[start[i]:(start[i]+len[i]-1)] <- count
    count <- count + 1
  }
  class
}