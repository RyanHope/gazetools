#' Gazetools Reference Class
#'
#' The Gazetools reference class is the main entry point for gaze classification and analysis.
#'
#' @field data an object of class \code{"data.table"} that holds gaze-related timeseries information.
#' @field samplerate the rate at which the gaze data were sampled.
#' @field rx the horizontal resolution of the screen (pixels)
#' @field ry the vertical resolution of the screen (pixels)
#' @field sw the physical screen width (mm)
#' @field sh the physical screen height (mm)
#'
#' @importFrom methods setRefClass
#' @importClassesFrom data.table data.table
#'
#' @examples
#' data(smi)
#' g <- with(smi, gazetools(smi_sxl,smi_syl, 500,
#'                          1680, 1050, 473.76, 296.1,
#'                          smi_ezl, smi_exl, smi_eyl,
#'                          blinks=(smi_dyl==0|smi_dyr==0)))
#'
#' g <- with(highspeed, gazetools(x,y,1250,1024,768,.38,.30,.67,
#'                                blinks=(x==0|y==0)))
#'
#' @name gazetools
#' @exportClass gazetools
#' @export gazetools
#'
gazetools <- setRefClass("gazetools",
                         fields=list(data="data.table", samplerate="numeric",
                                     rx="numeric", ry="numeric", sw="numeric", sh="numeric",
                                     window="numeric", timestamp="numeric",
                                     classifier="list"),
                         methods=list(initialize = function(..., import=NULL) {
                           "Initialize object with raw gaze data. Takes same arguments as \\code{\\link{pva}}."
                           if (class(import)=="gazetools") {
                             callSuper(import)
                           } else {
                             if ("pva" %in% class(import))
                               .self$data <- import
                             else
                              .self$data <- pva(...)

                             .self$samplerate <- attr(data,"samplerate")
                             .self$window <- attr(data,"window")
                             .self$rx <- attr(data,"rx")
                             .self$ry <- attr(data,"ry")
                             .self$sw <- attr(data,"sw")
                             .self$sh <- attr(data,"sh")

                             setattr(.self$data,"samplerate",NULL)
                             setattr(.self$data,"window",NULL)
                             setattr(.self$data,"rx",NULL)
                             setattr(.self$data,"ry",NULL)
                             setattr(.self$data,"sw",NULL)
                             setattr(.self$data,"sh",NULL)
                           }
                         })
)

#' @importFrom ggplot2 ggplot aes geom_point geom_path geom_segment facet_grid xlab ylab scale_size_continuous scale_color_manual scale_linetype_manual
#' @export
gazetools$methods(plot = function(filter, style="timeseries", background=NULL, rois=NULL) {
  if (style == "timeseries") {
    .call <- as.call(quote(.self$data[]))
    .call[[3]] <- match.call()$filter
    .labs <- c("Gaze X (px)","Gaze Y (px)","Velocity (deg/s)")
    .thresholds <- data.table(variable=c("Velocity (deg/s)",
                                     "Velocity (deg/s)"),
                              id=c("Saccade-peak",
                                   "Saccade-onset"),
                              y=c(.self$classifier$saccade_peak_threshold,
                                  .self$classifier$saccade_onset_threshold),
                              yend=c(.self$classifier$saccade_peak_threshold,
                                     .self$classifier$saccade_onset_threshold))
    .g <- eval(.call)[,list(time,x,y,v,class)][,list(variable=names(.SD),value=unlist(.SD,use.names=FALSE)),by=list(time,class)][,`:=`(variable=factor(variable,levels=c("x","y","v"),labels=.labs))]
    ggplot(.g) +
      geom_point(aes(x=time,y=value,color=class)) +
      geom_segment(data=.thresholds,aes(y=y,yend=y,x=-Inf,xend=Inf,group=id,linetype=id)) +
      facet_grid(variable~.,scales="free_y") +
      scale_linetype_manual("Threshold",values=c("dashed","dotted"),drop=TRUE,limits=c("Saccade-peak","Saccade-onset")) +
      scale_color_manual("Classification",values=c("cyan","black","red","orange","yellow"),drop=TRUE,limits=c("Noise","Fixation","Saccade","Glissade-fast","Glissade-slow"))
  } else if (style == "spatial-raw") {
    .call <- as.call(quote(.self$data[]))
    .call[[3]] <- match.call()$filter
    if (is.raster(background)) {
      p <- rplot(background) +
        geom_point(aes(x=x,y=y,color=class), data=eval(.call))
    } else {
      p <- ggplot(eval(.call)) +
        geom_point(aes(x=x,y=y,color=class))
    }
    p + xlab("x (px)") + ylab("y (px)") + coord_monitor(.self$rx,.self$ry)
  } else if (style == "spatial-class") {
    .call <- as.call(quote(.self$fixations()))
    .call[[2]] <- match.call()$filter
    d <- eval(.call)
    if (is.raster(background)) {
      p <- rplot(background) +
        geom_path(aes(x=fixation.x,y=fixation.y),linetype="dotted",data=d) +
        geom_point(aes(x=fixation.x,y=fixation.y, size=fixation.duration),alpha=.5,data=d) +
        scale_size_continuous(range = c(3,10))
    } else {
      p <- ggplot(d) +
        geom_path(aes(x=fixation.x,y=fixation.y),linetype="dotted") +
        geom_point(aes(x=fixation.x,y=fixation.y, size=fixation.duration),alpha=.5) +
        scale_size_continuous(range = c(3,10))
    }
    p + xlab("x (px)") + ylab("y (px)") + coord_monitor(.self$rx,.self$ry)
  }
})

#' @export
gazetools$methods(classify = function(vt=100,sigma=4.5) {
  minsac <- ((1/.self$samplerate)*ceiling(.self$window/2))
  .class <- gazetools::classify(.self$data[,v],.self$data[,blinks],.self$samplerate,vt,sigma,minsac,minsac*2)
  .self$classifier <- list(saccade_peak_threshold=attr(.class,"saccade-peak-threshold"),
                           saccade_onset_threshold=attr(.class,"saccade-onset-threshold"))
  invisible(.self$data[,class:=.class])
})
