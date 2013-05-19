# gazetools

The R package **gazetools** is a colletion of tools for processing and classifying eye gaze data.

## Installation

You can install the stable version on [CRAN](http://cran.r-project.org/package=gazetools):
  
  ```r
install.packages('gazetools', dependencies = TRUE)
```

Or download the [zip ball](https://github.com/ryanhope/gazetools/zipball/master) or 
[tar ball](https://github.com/ryanhope/gazetools/tarball/master), decompress and 
run `R CMD INSTALL` on it, or use the **devtools** package to install the 
absolutely latest version:
  
  ```r
## you may also need to update your packages: 
## options(repos = c(CRAN = 'http://cran.r-project.org'))
## update.packages()
library(devtools); install_github('gazetools', 'ryanhope')
```
## Visual Angle Functions

* subtendedAngle
* angle2pixels

## Threshold Algorithms

* mouldThreshold

## Classification Algorithms

* classify.V
* classify.VA
* classify.VI

## Position Measures

* chull_area
* voronoi_skewness
* extent
* bcea
* nni
* roi_coverage