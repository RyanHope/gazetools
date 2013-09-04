list(
  index = list(
    sd_section(
      "Data",
      "Example data sets.",
      c("smi","highspeed")
    ),
    sd_section(
      "Velocity and Acceleration Preprocessing",
      "This group of functions generates, manipulates or plots pva objects which
       contain velocity and acceleration profiles of raw gaze data. The pva object
       is key to using any of the velocity or acceleration based classification algorithms.",
      c("pva","pva-class","plot.pva","pva.as.data.frame")
    ),
    sd_section(
      "Utility",
      "These are handy utilty functions.",
      c("distance2point","subtendedAngle","angle2pixels","gridded_rois")
    ),
    sd_section(
      "Threshold Algorithms",
      "These functions are used to determine the optimal thresholds for use 
      in classification algorithms.",
      c("mouldThreshold")
    ),
    sd_section(
      "Classification Algorithms",
      "These functions are used to classify rag gaze data into events 
      such as saccades and fixations.",
      c("classify.V", "classify.VI", "classify.VA", "classify.MM")
    ),
    sd_section(
      "Position Measures",
      "These functions measure various aspects of the position of fixations.",
      c("chull_area","voronoi_skewness","extent","bounding_box","bcea","nni","roi_coverage")
    ),
    sd_section(
      "Plots",
      "Class plots using ggplot2",
      c("plot.mould","plot.classify","plot.voronoi_skewness")
    ),
    sd_section(
      "Geoms",
      "Geoms for ggplot2",
      c("geom_bounding_box","geom_chull","geom_roi_coverage","geom_voronoi","geom_grid","geom_png")
    ),
    sd_section(
      "Scales",
      "Scales for ggplot2",
      c("scale_grid")
    ),
    sd_section(
      "Classes",
      "Gazetools classes",
      c("mould-class","classify-class","gridded_rois-class","ROI-class","ROIs-class",
        "roi_coverage-class","voronoi_skewness-class")
    )
  ),
  icons = list(
    extent = sd_icon({
      gTree(children = gList(
        segmentsGrob(c(0.2,.5), c(0.5,.2), c(0.8,.5), c(0.5,.8)),
        segmentsGrob(c(0.4,.4), c(0.2,.8), c(0.6,.6), c(0.2,.8)),
        segmentsGrob(c(.2,.8),c(.4,.4),c(.2,.8),c(.6,.6))
      ))
    }),
    chull_area = sd_icon({
      d <- data.frame(x=c(0.24,0.46,0.81,0.06,0.84),y=c(0.57,0.86,0.24,0.10,0.37))
      hpts <- chull(d)
      hpts <- c(hpts, hpts[1])
      d <- d[hpts, ]
      polygonGrob(d$x,d$y,gp=gpar(fill="gray20"))
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
      d <- data.frame(x=c(0.24,0.46,0.81,0.06,0.84),y=c(0.57,0.86,0.24,0.10,0.37))
      hpts <- chull(d)
      hpts <- c(hpts, hpts[1])
      d <- d[hpts, ]
      polygonGrob(d$x,d$y)
    }),
    geom_voronoi = sd_icon({
      d <- data.frame(x=c(0.24,0.46,0.81,0.06,0.84),y=c(0.57,0.86,0.24,0.10,0.37))
      dd <- fortify(voronoi_polygons(d,c(0,1,0,1)))
      polygonGrob(dd$long,dd$lat,id=factor(dd$group))
    })
  )
)