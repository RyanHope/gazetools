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
    geom_voronoi = sd_icon({
      xpos <- c(1,1,2,3,3,3,4,4,5,5,5,5,6,7,7,7,8,8,9)/10
      ypos <- c(1,2,1,1,2,3,1,2,1,2,3,4,1,1,2,3,1,2,1)/10
      pointsGrob(x = xpos, y = ypos, pch = 19, size = unit(.1, "npc"),
                 gp = gpar(col = "black", cex = 0.5), default.units = "npc")
    })
  )
)