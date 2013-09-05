ROI <- function (coords, ID) {
  Polygons(list(Polygon(coords)), ID=ID)
}

ROIs <- function(rois) {
  SpatialPolygons(rois)
}