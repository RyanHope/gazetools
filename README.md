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

## Classification Algorithms

At the present time, the gazetools package has only implemented velocity and acceleration based classification algorithms.
In the future, dispersion based algorithms might be implements.

* classify.V - Classifies gaze data into saccades and fixations using a velocity threshold
* classify.VA - Classifies gaze data into saccades and fixations using both velocity and acceleration thresholds
* classify.VI - Classifies gaze data into saccades and fixations using an iteratively determined velocity threshold

## Threshold Algorithms

* mouldThreshold - Uses the Mould algorithm to determine the optimal velocity or acceleration threshold for classifying raw gaze data

## Position Measures

* voronoi_skewness - Computes the skewness of the areas of the voronoi cells surrounding fixations
