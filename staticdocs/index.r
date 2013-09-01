list(
  index = list(
    sd_section(
      "Data",
      "Example data sets.",
      c("smi")
    ),
    sd_section(
      "Utility",
      "These are handy utilty functions.",
      c("distance2point","subtendedAngle","angle2pixels","gridded_rois")
    ),
    sd_section(
      "Preprocessing",
      "These functions do necessary preprocessing operations.",
      c("pva")
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
      c("chull_area","voronoi_skewness","extent","bcea","nni","roi_coverage")
    ),
    sd_section(
      "Plots",
      "Class plots using ggplot2",
      c("plot.mould","plot.pva","plot.classify","plot.voronoi_skewness")
    ),
    sd_section(
      "Geoms",
      "Geoms for ggplot2",
      c("geom_bbox","geom_chull","geom_roi_coverage","geom_voronoi","geom_grid","geom_png")
    ),
    sd_section(
      "Scales",
      "Scales for ggplot2",
      c("scale_grid")
    ),
    sd_section(
      "Classes",
      "Gazetools classes",
      c("mould-class","pva-class","classify-class","gridded_rois-class",
        "roi_coverage-class","voronoi_skewness-class")
    )
  )
)