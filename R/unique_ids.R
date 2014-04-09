#' Unique IDs
#'
#' Assigns unique monotonically increasing numeric ids to runs of identical elements in a vector.
#'
#' @param class a vector
#'
#' @importFrom Rmisc rsi
#'
#' @export
#'
#' @examples
#' unique_ids(c("a","a","b","a","a","a","c","c"))
#'
unique_ids <- function(class) {
  class <- as.numeric(factor(class,labels=1:length(unique(factor(class)))))
  start <- rsi(class)
  len <- rle(class)$lengths
  count <- 1
  for (i in 1:length(start)) {
    class[start[i]:(start[i]+len[i]-1)] <- count
    count <- count + 1
  }
  class
}