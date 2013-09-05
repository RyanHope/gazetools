require(grid)
require(ggplot2)

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
    })
  )
)