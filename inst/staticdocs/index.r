sd_section(
  "Data",
  "Example data sets.",
  c(
    "lc120",
    "smi",
    "highspeed"
    )
)
sd_section(
  "Utility",
  "These are handy utilty functions.",
  c(
    "rplot",
    "event_ids",
    "unique_ids",
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
  "Class Plot Methods",
  "Functions for plotting classes used in the gazetools package.",
  c(
    "plot,pva,classify-method",
    "plot,pva,missing-method"
  )
)
sd_section(
  "Classes",
  "Classes used in the gazetools package",
  c(
    "pva-class",
    "classify-class",
    "mould-class"
  )
)