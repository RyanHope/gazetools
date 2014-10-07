pad_blinks <- function(x, pad) {
  N <- length(x)
  out <- rep(FALSE, N)
  for (i in which(x)) out[pmax(i-pad,1):pmin(i+pad,N)] <- TRUE
  out
}

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
#' @importFrom zoo na.spline
#' @importFrom signal filter sgolay
#' @importClassesFrom data.table data.table
#'
#' @examples
#' data(smi)
#' g <- with(smi, gazetools(smi_sxl, smi_syl, 500, 1680, 1050, 473.76, 296.1, smi_ezl, smi_exl, smi_eyl))
#'
#' @exportClass gazetools
#' @export gazetools
#'
gazetools <- setRefClass("gazetools",
                         fields=list(data="data.table", samplerate="numeric",
                                     rx="numeric", ry="numeric", sw="numeric", sh="numeric",
                                     window="numeric", timestamp="numeric"),
                         methods=list(initialize = function(x, y, samplerate, rx, ry, sw, sh, ez,
                                                            ex = 0, ey = 0, timestamp = -1, order = 2, window = 19,
                                                            vt=1000, at=100000, blinks = NULL, import=NULL) {
                           if (!is.null(import)) {
                             callSuper(import)
                           } else {

                             ts <- 1 / samplerate
                             filter.velocity <- sgolay(p =  order, n = window, m = 1, ts = ts)
                             filter.acceleration <- sgolay(p =  order, n = window, m = 2, ts = ts)

                             N <- length(x)
                             cx <- rep(rx / 2, N)
                             cy <- rep(ry /2, N)

                             .self$data <- data.table(time = 0:(N-1) * ts, timestamp = timestamp,
                                                      x = x, y = y, ez = ez, ex = ex, ey = ey)

                             if (!is.null(blinks)) {
                               .self$data[,blinks:=pad_blinks(blinks, 30)]
                               .self$data[blinks==TRUE,`:=`(x=NA,y=NA)]
                             } else {
                               .self$data[,blinks:=FALSE]
                             }
                             .self$data[,`:=`(x=na.spline(x, na.rm=FALSE),
                                              y=na.spline(y, na.rm=FALSE))]

                             xa <- .self$data[,na.spline(subtended_angle(x, cy, cx, cy, rx, ry, sw, sh, ez, ex, ey), na.rm=FALSE)]
                             ya <- .self$data[,na.spline(subtended_angle(cx, y, cx, cy, rx, ry, sw, sh, ez, ex, ey), na.rm=FALSE)]

                             vx <- filter(filter.velocity, xa)
                             vy <- filter(filter.velocity, ya)

                             ax <- filter(filter.acceleration, xa)
                             ay <- filter(filter.acceleration, ya)

                             .self$data[,`:=`(v=sqrt(vx**2 + vy**2),
                                              a=sqrt(ax**2 + ay**2))]

                             .self$data[v>vt | a>at, `:=`(x=NA,y=NA,v=NA,a=NA)]

                             filter.smooth <- sgolay(p =  order, n = window, m = 0, ts = ts)
                             .self$data[,`:=`(x=filter(filter.smooth, (na.spline(x, na.rm=FALSE))),
                                              y=filter(filter.smooth, (na.spline(y, na.rm=FALSE))),
                                              v=na.spline(v, na.rm=FALSE),
                                              a=na.spline(a, na.rm=FALSE))]

                             .self$rx <- rx
                             .self$ry <- ry
                             .self$sw <- sw
                             .self$sh <- sh
                             .self$samplerate <- samplerate
                           }
                         })
)

#' @importFrom ggplot2 ggplot aes geom_point geom_path facet_grid xlab ylab scale_size_continuous
gazetools$methods(plot = function(filter, style="timeseries", background=NULL, rois=NULL) {
  if (style == "timeseries") {
    .call <- as.call(quote(.self$data[]))
    .call[[3]] <- match.call()$filter
    g <- eval(.call)
    g[,class:=factor(class,levels=c("BLINK","FIXATION","SACCADE"))]
    ggplot(g[,list(time,x,y,v,a,class)][,list(variable=names(.SD),value=unlist(.SD,use.names=FALSE)),by=list(time,class)][,`:=`(variable=factor(variable,levels=c("x","y","v","a"),labels=c("Gaze X (px)","Gaze Y (px)","Velocity (deg/s)","Acceleration (deg/2^2")))]) +
      geom_point(aes(x=time,y=value,color=class)) +
      facet_grid(variable~.,scales="free_y") +
      scale_color_manual(values=c("cyan","black","red"),drop=TRUE,limits=c("BLINK","FIXATION","SACCADE"))
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
gazetools$methods(classify = function(vt=100,sigma=3) {
  invisible(.self$data[,class:=gazetools::classify(v,blinks,vt,sigma)])
})
