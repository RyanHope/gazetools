setClass("ROI", representation(ID="character"), contains="Polygon")

ROI <- function (coords, ID) {
  new("ROI", Polygon(coords), ID=ID)
}

setClass("ROIs", contains="list", validity=function(object) {
  !any(sapply(object, function(x) !is(x, "ROI")))
})

ROIs <- function(rois) {
  stopifnot(is.list(rois))
  stopifnot(length(rois) > 0)
  names(rois) <- sapply(rois, function(x) x@ID)
  if (any(sapply(rois, function(x) !is(x, "ROI")))) 
    stop("rois not a list of ROI objects")
  new("ROIs", rois)
}

fortify.ROIs <- function(model, data, ...) {
  df <- ldply(model, function(model) as.data.frame(model@coords))
  colnames(df) <- c("id","x","y")
  df
}