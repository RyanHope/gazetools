utils::globalVariables(c("to","from"))

#' Get Transition Matrix
#'
#' Calculates a transition matrix from a scanpath.
#' 
#' @param scanpath a vector a ROI ids (aka scanpath)
#' @param expected if TRUE calculate the expected frequencies
#' @param chisquare if TRUE calculate the chi-squared statistic
#' @param prob if TRUE convert frequencies into probabilities
#' @param digits the number of digits to round to
#'
#' @importFrom sp point.in.polygon
#' @importFrom reshape2 dcast
#'
#' @return an object of class \code{\linkS4class{Scanpath}}
#'
#' @export
#' 
getTransitionMatrix <- function(scanpath, expected=F, chisquare=F,prob=F,digits=3) {
  tmp <- ddply(data.frame(from=head(scanpath,-1),to=tail(scanpath,-1)), .(from, to), function(x) {
    data.frame(count=nrow(x))
  })
  tmp <- dcast(tmp, from~to, value.var="count")
  tmp[is.na(tmp)] <- 0
  rownames(tmp) <- tmp$from
  tmp$from <- NULL
  if (expected || chisquare) {
    tmp2 <- tmp
    N <- sum(tmp2)
    rs <- rowSums(tmp2)
    cs <- colSums(tmp2)
    for (.r in rownames(tmp2)) {
      for (.c in colnames(tmp2)) {
        tmp2[.r,.c] <- round((rs[.r]*cs[.c])/N)
      }
    }
    if (chisquare) {
      tmp3 <- tmp2
      for (.r in rownames(tmp3)) {
        for (.c in colnames(tmp3)) {
          tmp3[.r,.c] <- ((tmp[.r,.c]-tmp2[.r,.c])**2)/tmp2[.r,.c]
          if (is.nan(tmp3[.r,.c]) || is.infinite(tmp3[.r,.c])) {
            tmp3[.r,.c] <- NA
          } else {
            tmp3 <- round(tmp3, digits)
          }
        }
      }
      tmp <- tmp3
    } else {
      tmp <- tmp2
    }
  }
  tmp
}