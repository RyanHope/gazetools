#' @export
initIDs <- function(base=NULL) {
  op <- options()
  op.gazetools <- list(
    gazetools.last.fixationid = 0,
    gazetools.last.saccadeid = 0,
    gazetools.last.glissadeslowid = 0,
    gazetools.last.glissadefastid = 0,
    gazetools.last.blinkid = 0,
    gazetools.last.eventid = 0
  )
  toset <- !(names(op.gazetools) %in% names(op))
  if(any(toset)) options(op.gazetools[toset])
}

.onLoad <- function(libname, pkgname) {
  
  
  invisible()
}

.onUnload <- function(libname, pkgname) {
  op <- options()
  op.gazetools <- list(
    gazetools.last.fixationid = 0,
    gazetools.last.saccadeid = 0,
    gazetools.last.glissadeslowid = 0,
    gazetools.last.glissadefastid = 0,
    gazetools.last.blinkid = 0,
    gazetools.last.eventid = 0
  )
  toset <- !(names(op.gazetools) %in% names(op))
  if(any(toset)) options(op.gazetools[toset])
  
  invisible()
}