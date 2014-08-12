#' Empty Theme
#' 
#' Empty ggplot2 theme
#' 
#' @importFrom ggplot2 theme element_blank element_text
#' 
#' @export
#' 
theme_empty <- function() 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.title.x = element_text(colour = NA), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank())

#' Monitor Coordinates
#' 
#' Sets the ggplot apect ration and limits to the bounds of a computer monitor.
#' 
#' @param res_width the x resolution of a monitor
#' @param res_height the y resolution of a monitor
#' 
#' @importFrom ggplot2 coord_fixed
#' 
#' @export
#' 
coord_monitor <- function(res_width,res_height) coord_fixed(xlim=c(0,res_width),ylim=c(0,res_height))