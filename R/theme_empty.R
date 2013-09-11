#' @import ggplot2
theme_empty <- function() 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.title.x = element_text(colour = NA), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank())