require(grid)
require(ggplot2)

list(
  index = list(
    sd_section(
      "Data",
      "Example data sets.",
      c(
        "smi",
        "highspeed"
        )
    ),
    sd_section(
      "Preprocessing and Filtering",
      "These functions do necessary preprocessing and filtering operations.",
      c(
        "pva"
        )
    ),
    sd_section(
      "Utility",
      "These are handy utilty functions.",
      c(
        "distance2point",
        "subtendedAngle",
        "angle2pixels",
        "gridded_rois"
        )
    ),
    sd_section(
      "Threshold Algorithms",
      "These functions are used to determine the optimal thresholds for use 
      in classification algorithms.",
      c(
        "mouldThreshold"
        )
    ),
    sd_section(
      "Classification Algorithms",
      "These functions are used to classify rag gaze data into events 
      such as saccades and fixations.",
      c(
        "classify.V",
        "classify.VI",
        "classify.VA",
        "classify.MM"
        )
    ),
    sd_section(
      "Regions of Interest",
      "Helper functions for defining polygon based regions/areas of interest.",
      c(
        "ROI",
        "ROIs"
      )
    ),
    sd_section(
      "Position Measures",
      "These functions measure various aspects of the position of fixations.",
      c(
        "chull_area",
        "voronoi_skewness",
        "extent",
        "bcea",
        "nni",
        "roi_coverage",
        "Scanpath",
        "Coverage"
        )
    ),
    sd_section(
      "Plots",
      "Class plots using ggplot2",
      c(
        "plot.pva",
        "plot.mould",
        "plot.classify",
        "plot.voronoi_skewness"
        )
    ),
    sd_section(
      "Fortification",
      "Fortify methods convert R objects into data frames usefull for plotting.",
      c(
        "fortify.ROIs",
        "fortify.Scanpath",
        "fortify.Coverage"
      )
    ),
    sd_section(
      "Geoms",
      "Geoms for ggplot2",
      c(
        "geom_boundingbox",
        "geom_ellipse",
        "geom_chull",
        "geom_knn",
        "geom_roi_coverage",
        "geom_voronoi",
        "geom_grid",
        "geom_png"
        )
    ),
    sd_section(
      "Stats",
      "Stats for ggplot2",
      c(
        "stat_boundingbox",
        "stat_ellipse",
        "stat_knn"
      )
    ),
    sd_section(
      "Scales",
      "Scales for ggplot2",
      c(
        "scale_grid"
        )
    ),
    sd_section(
      "Classes",
      "Gazetools classes",
      c(
        "pva-class",
        "mould-class",
        "classify-class",
        "gridded_rois-class",
        "ROI-class",
        "ROIs-class",
        "Scanpath-class",
        "Coverage-class",
        "voronoi_skewness-class"
        )
    )
  ),
  icons = list(
    circleFun =sd_icon({
      d <- circleFun(c(.5,.5),.8,npoints=17)
      pointsGrob(x=d$x,y=d$y,pch=19,gp=gpar(cex=.25),default.units="npc")
    }),
    classify.V = sd_icon({
      textGrob("Class\nV",gp=gpar(fontface="bold"))
    }),
    classify.VA = sd_icon({
      textGrob("Class\nVA",gp=gpar(fontface="bold"))
    }),
    classify.VI = sd_icon({
      textGrob("Class\nVI",gp=gpar(fontface="bold"))
    }),
    classify.MM = sd_icon({
      textGrob("Class\nMM",gp=gpar(fontface="bold"))
    }),
    smi = sd_icon({
      textGrob("500\nHz",gp=gpar(fontface="bold"))
    }),
    highspeed = sd_icon({
      textGrob("1250\nHz",gp=gpar(fontface="bold"))
    }),
    bcea = sd_icon({
      d <- data.frame(x=c(0.30,0.17,0.25,0.53,0.28,0.82,0.51,0.48,0.56,0.77),
                      y=c(0.48,0.41,0.40,0.29,0.27,0.79,0.65,0.43,0.94,0.43))
      b <- bceFun(d,.75)
      gTree(children = gList(
        polygonGrob(b$x,b$y,gp=gpar(fill="gray",alpha=.5)),
        pointsGrob(x=d$x,y=d$y,pch=19,gp=gpar(cex=.25),default.units="npc")
      ))
    }),
    extent = sd_icon({
      gTree(children = gList(
        segmentsGrob(c(0.2,.5), c(0.5,.2), c(0.8,.5), c(0.5,.8)),
        segmentsGrob(c(0.4,.4), c(0.2,.8), c(0.6,.6), c(0.2,.8)),
        segmentsGrob(c(.2,.8),c(.4,.4),c(.2,.8),c(.6,.6))
      ))
    }),
    chull_area = sd_icon({
      d <- data.frame(x=c(0.30,0.17,0.25,0.53,0.28,0.82,0.51,0.48,0.56,0.77),
                      y=c(0.48,0.41,0.40,0.29,0.27,0.79,0.65,0.43,0.94,0.43))
      hpts <- chull(d)
      hpts <- c(hpts, hpts[1])
      b <- d[hpts, ]
      gTree(children = gList(
        polygonGrob(b$x,b$y,gp=gpar(fill="gray",alpha=.5)),
        pointsGrob(x=d$x,y=d$y,pch=19,gp=gpar(cex=.25),default.units="npc")
      ))
    }),
    plot.voronoi_skewness = sd_icon({
      d <- data.frame(x=c(0.24,0.46,0.81,0.06,0.84),y=c(0.57,0.86,0.24,0.10,0.37))
      dd <- fortify(voronoi_polygons(d,c(0,1,0,1)))
      gTree(children = gList(
        polygonGrob(dd$long,dd$lat,id=factor(dd$group)),
        pointsGrob(x=d$x,y=d$y,pch=19,gp=gpar(cex=.25),default.units="npc")
      ))
    }),
    geom_chull = sd_icon({
      d <- data.frame(x=c(0.30,0.17,0.25,0.53,0.28,0.82,0.51,0.48,0.56,0.77),
                      y=c(0.48,0.41,0.40,0.29,0.27,0.79,0.65,0.43,0.94,0.43))
      hpts <- chull(d)
      hpts <- c(hpts, hpts[1])
      b <- d[hpts, ]
      gTree(children = gList(
        polygonGrob(b$x,b$y),
        pointsGrob(x=d$x,y=d$y,pch=19,gp=gpar(cex=.25),default.units="npc")
      ))
    }),
    geom_voronoi = sd_icon({
      d <- data.frame(x=c(0.24,0.46,0.81,0.06,0.84),y=c(0.57,0.86,0.24,0.10,0.37))
      dd <- fortify(voronoi_polygons(d,c(0,1,0,1)))
      polygonGrob(dd$long,dd$lat,id=factor(dd$group))
    }),
    geom_knn = sd_icon({
      d <- data.frame(x=c(0.24,0.46,0.81,0.06,0.84,.4,.2),y=c(0.57,0.86,0.24,0.10,0.37,.5,.2))
      d <- cbind(d,d[nnwhich(d),])
      colnames(d) <- c(x="x",y="y",xend="xend",yend="yend")
      gTree(children = gList(
        segmentsGrob(d$x,d$y,d$xend,d$yend),
        pointsGrob(d$x,d$y,pch=19,gp=gpar(cex=.25),default.units="npc")
      ))
    }),
    nni = sd_icon({
      d <- data.frame(x=c(0.24,0.46,0.81,0.06,0.84,.4,.2),y=c(0.57,0.86,0.24,0.10,0.37,.5,.2))
      d <- cbind(d,d[nnwhich(d),])
      colnames(d) <- c(x="x",y="y",xend="xend",yend="yend")
      gTree(children = gList(
        segmentsGrob(d$x,d$y,d$xend,d$yend),
        pointsGrob(d$x,d$y,pch=19,gp=gpar(cex=.25),default.units="npc")
      ))
    }),
    voronoi_skewness = sd_icon({
      y <- c(0.45,.7, 0.8, 0.5, 0.3, 0.2, 0.125, 0.125,.1)
      rectGrob(seq(0.1, 0.9, by = 0.1), y, height = y, width = 0.1, vjust = 1,
               gp = gpar(fill = "grey20", col = NA))
    }),
    local_maxima = sd_icon({
      x <- seq(-3,3,.3)
      y <- sin(x+pi/2)*cos(2*x)
      col <- rep("black",length(x))
      col[local_maxima(y)] <- "red"
      pointsGrob((x+3)/6,(y+1)/2.1,pch=19,gp=gpar(cex=.25,col=col),default.units="npc")
    }),
    find_peaks = sd_icon({
      x=seq(-.5,3,.25)
      y=sin(x+pi/2)*cos(2*x)
      col <- rep("black",length(x))
      col[3] = "red"
      gTree(children = gList(
        pointsGrob((x+.5)/3.5*.8+.1,(y+1)/2*.8+.1,pch=19,gp=gpar(cex=.25,col=col),default.units="npc"),
        segmentsGrob(0,.65,1,.65)
      ))
    }),
    find_peak_ranges = sd_icon({
      x=seq(-.5,3,.25)
      y=sin(x+pi/2)*cos(2*x)
      col <- rep("black",length(x))
      col[1:5] = "red"
      gTree(children = gList(
        pointsGrob((x+.5)/3.5*.8+.1,(y+1)/2*.8+.1,pch=19,gp=gpar(cex=.25,col=col),default.units="npc"),
        segmentsGrob(0,.65,1,.65)
      ))
    })
  )
)