sd_section(
  "Data",
  "Example data sets.",
  c(
    "lc120",
    "smi",
    "highspeed",
    "logo"
    )
)
sd_section(
  "Classes",
  "Classes used in the gazetools package",
  c(
    "pva-class",
    "classify-class",
    "mould-class",
    "ROI-class",
    "ROIs-class",
    "DynamicROI-class",
    "DynamicROIs-class",
    "VoronoiPolygons-class",
    "Coverage-class",
    "Scanpath-class"
  )
)
sd_section(
  "Utility",
  "These are handy utilty functions.",
  c(
    "event_ids",
    "unique_ids",
    "local_maxima",
    "find_peak_ranges",
    "find_peaks",
    "distance_2_point",
    "subtended_angle"
  )
)
sd_section(
  "Preprocessing",
  "Preprocessing functions for use before classification.",
  c(
    "mould",
    "detect_blinks.PV",
    "detect_blinks.SW",
    "pva"
    )
)
sd_section(
  "Classification Algorithms",
  "Functions for classifying raw gaze data into events like saccades and fixations.",
  c(
    "classify.V",
    "classify.VI",
    "classify.VA",
    "classify.MM"
    )
)
sd_section(
  "Event Extraction",
  "Functions to extract events from classied gaze data.",
  c(
    "getFixations",
    "getSaccades",
    "getGlissades" 
  )
)
sd_section(
  "Regions of Interest",
  "Functions for defining regions of interest used in event analysis.",
  c(
    "ROI",
    "ROIs",
    #"DynamicROI",
    #"DynamicROIs",
    "gridded_rois"
  )
)
sd_section(
  "Event Analysis",
  "Functions to analyze gaze events.",
  c(
    "getScanpath",
    "getTransitionMatrix"
  )
)
sd_section(
  "Class Plot Methods",
  "Functions for plotting classes used in the gazetools package.",
  c(
    "plot,pva,classify-method",
    "plot,pva,missing-method",
    "plot.classify",
    "plot.mould",
    "plot.ROIs"
  )
)
sd_section(
  "Misc ggplot",
  "These are handy ggplot2 related functions.",
  c(
    "rplot",
    "coord_monitor"
  )
)