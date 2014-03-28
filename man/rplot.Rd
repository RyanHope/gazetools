\name{rplot}
\alias{rplot}
\title{Raster Plot}
\usage{
rplot(raster, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{raster}{a rasterized image}

  \item{xlim}{a vector of length 2 that contains the min
  and max horizontal bounds of the image}

  \item{ylim}{a vector of length 2 that contains the min
  and max vertical bounds of the image}
}
\description{
A wrapper around a few ggplot2 functions that creates a base plot layer out of a raster image.
Coordinates are restricted to values within the raster image and the aspect ratio is fixed.
}
\examples{
require(png)
r <- as.raster(readPNG(system.file("logo.png", package="gazetools")))
rplot(r)
}

