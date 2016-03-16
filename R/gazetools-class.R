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
#'                          blinks=(smi_dyl==0|smi_dyr==0),
#'                          timestamp=smi_time))
#' g
#'
#' g <- with(highspeed, gazetools(x,y,1250,1024,768,.38,.30,.67,
#'                                blinks=(x==0|y==0)))
#' g
#'
#' @name gazetools-class
#' @rdname gazetools-class
#' @exportClass gazetools
#' @export gazetools
#'
gazetools <- setRefClass("gazetools",
                         fields=list(data="data.table", samplerate="numeric",
                                     rx="numeric", ry="numeric", sw="numeric", sh="numeric",
                                     window="numeric", timestamp="numeric", minsac="numeric",
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
                             .self$minsac <- attr(data,"minsac")
                             .self$rx <- attr(data,"rx")
                             .self$ry <- attr(data,"ry")
                             .self$sw <- attr(data,"sw")
                             .self$sh <- attr(data,"sh")

                             setattr(.self$data,"samplerate",NULL)
                             setattr(.self$data,"window",NULL)
                             setattr(.self$data,"minsac",NULL)
                             setattr(.self$data,"rx",NULL)
                             setattr(.self$data,"ry",NULL)
                             setattr(.self$data,"sw",NULL)
                             setattr(.self$data,"sh",NULL)
                           }
                         })
)

#' @importFrom ggplot2 ggplot aes geom_point geom_path geom_segment facet_wrap xlab ylab scale_size_continuous scale_size_manual scale_color_manual scale_linetype_manual
#' @export
gazetools$methods(plot = function(filter, style="timeseries", background=NULL, show.thresholds=TRUE, all.classes=TRUE, show.quality.guide=TRUE) {
  if (style == "timeseries") {
    .call <- as.call(quote(.self$data[]))
    .call[[3]] <- match.call()$filter
    .labs <- c("Gaze X (px)","Gaze Y (px)","Total Velocity (°/s)")
    .g <- eval(.call)
    cx <- .self$rx/2 + mean(.g$ex)
    cy <- .self$ry/2 + mean(.g$ey)
    .thresholds <- data.table(x=rep(-Inf,2),
                              xend=rep(Inf,2),
                              y=c(.self$classifier$saccade_peak_threshold,
                                  .self$classifier$saccade_onset_threshold),
                              yend=c(.self$classifier$saccade_peak_threshold,
                                     .self$classifier$saccade_onset_threshold),
                              variable=c("Total Velocity (°/s)",
                                         "Total Velocity (°/s)"),
                              id=c("Saccade-peak",
                                   "Saccade-onset"))
    .classes <- data.table(values=c("cyan","magenta","black","red","orange","yellow"),
                           limits=c("Noise","Blink","Fixation","Saccade","Glissade-fast","Glissade-slow"))
    if (!all.classes)
      .classes <- .classes[limits %in% .g[,unique(class)]]
    .g <- .g[,list(time,sx,sy,v,class,quality)][,list(variable=names(.SD),value=unlist(.SD,use.names=FALSE)),by=list(time,class,quality)][,`:=`(variable=factor(variable,levels=c("sx","sy","v"),labels=.labs))]
    p <- ggplot(.g) +
      geom_point(aes(x=time,y=value,color=class,size=factor(quality))) +
      facet_wrap(~variable,ncol=1,scales="free_y") +
      scale_color_manual("Classification",values=.classes[,values],drop=TRUE,limits=.classes[,limits]) +
      xlab("Time (s)") + ylab("Value")
    if (!show.quality.guide)
      p <- p + scale_size_manual("Quality",values=c(.5,1,2),limits=c(0,0.5,1),guide=FALSE)
    else
      p <- p + scale_size_manual("Quality",values=c(.5,1,2),limits=c(0,0.5,1))
    if (show.thresholds)
      p <- p +
      geom_segment(aes(y=y,yend=y,x=-Inf,xend=Inf,group=id,linetype=id),.thresholds) +
      scale_linetype_manual("Threshold",values=c("dashed","dotted"),drop=TRUE,limits=c("Saccade-peak","Saccade-onset"))
    p
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
gazetools$methods(classify = function(vt=100,sigma=4,minblink=.08,glswin=.04,alpha=.7,idbase=0) {
  .class <- gazetools::classify(.self$data[,v],.self$data[,blinks],.self$samplerate,vt,sigma,minblink,.self$minsac,glswin,alpha)
  .self$classifier <- list(saccade_peak_threshold=attr(.class,"saccade-peak-threshold"),
                           saccade_onset_threshold=attr(.class,"saccade-onset-threshold"),
                           sigma=attr(.class,"sigma"),
                           minblink=attr(.class,"minblink"),
                           minsac=attr(.class,"minsac"),
                           glswin=attr(.class,"glswin"),
                           alpha=attr(.class,"alpha"))
  .self$data[,`:=`(class=.class)]
  .self$data[,`:=`(event_ids=ruidvec(as.character(class), idbase),
                   fixation_ids=uidvec(class=="Fixation", idbase),
                   saccade_ids=uidvec(class=="Saccade", idbase),
                   glissade_slow_ids=uidvec(class=="Glissade-slow", idbase),
                   glissade_fast_ids=uidvec(class=="Glissade-fast", idbase),
                   blink_ids=uidvec(class=="Blink", idbase))]
  .self$data[,quality:=1]
  blink_event_ids <- .self$data[blink_ids!=0,unique(event_ids)]
  .self$data[event_ids %in% (blink_event_ids-1),quality:=.5]
  .self$data[event_ids %in% (blink_event_ids+1),quality:=.5]
  .self$data[event_ids %in% (blink_event_ids),quality:=0]
  invisible(.class)
})

#' @export
gazetools$methods(fixations = function(filter, by=NULL) {
  .call <- as.call(quote(.self$data[]))
  .call[[3]] <- match.call()$filter
  .by <- c(by,"fixation_ids")
  eval(.call)[fixation_ids>0, list(event_ids=.SD[1,event_ids],
                                   time.begin=.SD[1,time],
                                   time.end=.SD[.N,time],
                                   timestamp.begin=.SD[1,timestamp],
                                   timestamp.end=.SD[.N,timestamp],
                                   fixation.x=round(mean(.SD[,x])),
                                   fixation.y=round(mean(.SD[,y])),
                                   fixation.duration=(.N)*(1/.self$samplerate),
                                   fixation.velocity=mean(.SD[,v]),
                                   ex=round(mean(.SD[,ex])),
                                   ey=round(mean(.SD[,ey])),
                                   ez=round(mean(.SD[,ez])),
                                   quality=mean(.SD[,quality])),
              by=.by][,{options(gazetools.lastfixid = .SD[.N,fixation_ids]); .SD}]
})

#' @importFrom pastecs turnpoints
#' @export
gazetools$methods(saccades = function(filter, by=NULL) {
  .call <- as.call(quote(.self$data[]))
  .call[[3]] <- match.call()$filter
  .by <- c(by,"saccade_ids")
  eval(.call)[saccade_ids>0 & !(saccade_ids==1 & event_ids==1), list(event_ids=.SD[1,event_ids],
                                                                     time.begin=.SD[1,time],
                                                                     time.end=.SD[.N,time],
                                                                     timestamp.begin=.SD[1,timestamp],
                                                                     timestamp.end=.SD[.N,timestamp],
                                                                     preceding.fixation.duration=.self$data[event_ids==.SD[1,event_ids-1],ifelse(.SD[1,class]=="Fixation", .SD[.N,time]-.SD[1,time], NA_real_)],
                                                                     saccade.amplitude=subtended_angle(.SD[1,sx],.SD[1,sy],.SD[.N,sx],.SD[.N,sy],.self$rx,.self$ry,.self$sw,.self$sh,.SD[,mean(ez)],.SD[,mean(ex)],.SD[,mean(ey)]),
                                                                     saccade.duration=.N/.self$samplerate,
                                                                     saccade.peak_velocity=max(.SD[,v]),
                                                                     saccade.mean_velocity=mean(.SD[,v]),
                                                                     ex=round(mean(.SD[,ex])),
                                                                     ey=round(mean(.SD[,ey])),
                                                                     ez=round(mean(.SD[,ez])),
                                                                     quality=mean(.SD[,quality]),
                                                                     xcross=(.SD[1,sx]<.self$rx/2 && .self$rx/2<.SD[.N,sx]),
                                                                     ycross=(.SD[1,sy]<.self$ry/2 && .self$ry/2<.SD[.N,sy]),
                                                                     peaks=suppressWarnings(length(which(turnpoints(.SD[,v])$peaks)))),
              by=.by][,{options(gazetools.saccadeid = .SD[.N,saccade_ids]); .SD}]
})

#' @export
gazetools$methods(scanpath = function(filter, rois, threshold=2, by=NULL) {
  .bbl <- .self$data[,floor(min(x))]
  .bbr <- .self$data[,ceiling(max(x))]
  .bbb <- .self$data[,floor(min(y))]
  .bbt <- .self$data[,ceiling(max(y))]
  .na <- data.table(id="unknown",parent="unknown",layer=-Inf,start=.self$data[1,timestamp],end=.self$data[.N,timestamp],static=T,x=list(c(.bbl,.bbr,.bbr,.bbl,.bbl)),y=list(c(.bbb,.bbb,.bbt,.bbt,.bbb)))
  .call <- as.call(quote(.self$fixations()))
  .call[[2]] <- match.call()$filter
  .call$by = by
  fix <- eval(.call)
  .by <- c(by,"fixation_ids")
  .rois <- rbindlist(list(.na,rois))
  setkeyv(fix,.by)
  setkey(.rois, start, end)
  merge(fix, fix[,.rois[start<timestamp.begin & timestamp.begin<=end,
                        list(roi.layer=layer,roi.x=x,roi.y=y,
                             size=subtended_angle(min(unlist(x)),min(unlist(y)),max(unlist(x)),max(unlist(y)),.self$rx,.self$ry,.self$sw,.self$sh,ez),
                             dist=subtended_angle(fixation.x,fixation.y,mean(tail(unlist(x),-1)),mean(tail(unlist(y),-1)),.self$rx,.self$ry,.self$sw,.self$sh,ez),
                             pip=point.in.polygon(fixation.x,fixation.y,unlist(x),unlist(y))), by=c("id","parent")][((roi.layer<0 & pip==1)|(roi.layer>=0 & (dist-size/2)<threshold))][order(-roi.layer,dist)][1],by=.by],by=.by)
})

#' @export
summary.gazetools <- function(g, by=NULL) {
  g[,rbindlist(list(
    .SD[quality==1 & fixation_ids!=0,list(duration=.SD[.N,time]-.SD[1,time]),by=fixation_ids][,list(measure="Fixation dur. (ms)",value=mean(duration)*1000, sd=sd(duration)*1000, N=.N)],
    .SD[quality==1 & saccade_ids!=0,list(duration=.SD[.N,time]-.SD[1,time]),by=saccade_ids][,list(measure="Saccade dur. (ms)",value=mean(duration)*1000, sd=sd(duration)*1000, N=.N)],
    .SD[quality==1 & glissade_slow_ids!=0,list(duration=.SD[.N,time]-.SD[1,time]),by=glissade_slow_ids][,list(measure="Glissade-slow dur. (ms)",value=mean(duration)*1000, sd=sd(duration)*1000, N=.N)],
    .SD[quality==1 & glissade_fast_ids!=0,list(duration=.SD[.N,time]-.SD[1,time]),by=glissade_fast_ids][,list(measure="Glissade-fast dur. (ms)",value=mean(duration)*1000, sd=sd(duration)*1000, N=.N)],
    .SD[quality==1 & fixation_ids!=0,list(velocity=mean(v)),by=fixation_ids][,list(measure="Fixation vel. (°/s)",value=mean(velocity), sd=sd(velocity), N=.N)],
    .SD[quality==1 & saccade_ids!=0,list(velocity=max(v)),by=saccade_ids][,list(measure="Saccade peak vel. (°/s)",value=mean(velocity), sd=sd(velocity), N=.N)],
    .SD[quality==1 & saccade_ids!=0,list(acceleration=max(a)),by=saccade_ids][,list(measure="Saccade peak acc. (°/s²)",value=mean(acceleration), sd=sd(acceleration), N=.N)],
    .SD[,list(measure="Max peak vel. (°/s)",value=max(v), sd=NA, N=1)],
    .SD[,list(measure="Max peak acc (°/s)",value=max(a), sd=NA, N=1)],
    .SD[,list(measure="% glissadic saccades",value=100*((max(glissade_slow_ids)+max(glissade_fast_ids))/max(saccade_ids)), sd=NA, N=1)],
    .SD[,list(blinks=.SD[(class=="Blink"),.N]/.N)][,list(measure="% blinks",value=100*blinks,sd=NA,N=1)],
    .SD[,list(noise=.SD[(class=="Noise"),.N]/.N)][,list(measure="% noise",value=100*noise,sd=NA,N=1)]
  )),by=by]
}

summary.gazetools2 <- function(g, by=NULL) {
  g[,rbindlist(list(
    .SD[quality==1 & fixation_ids!=0,list(duration=.SD[.N,time]-.SD[1,time]),by=fixation_ids][,list(Measure="Fixation dur. (ms)",Summary=sprintf("%.01f ± %.01f, (N = %d)", mean(duration)*1000, sd(duration)*1000, .N))],
    .SD[quality==1 & saccade_ids!=0,list(duration=.SD[.N,time]-.SD[1,time]),by=saccade_ids][,list(Measure="Saccade dur. (ms)",Summary=sprintf("%.01f ± %.01f, (N = %d)", mean(duration)*1000, sd(duration)*1000, .N))],
    .SD[quality==1 & glissade_slow_ids!=0,list(duration=.SD[.N,time]-.SD[1,time]),by=glissade_slow_ids][,list(Measure="Glissade-slow dur. (ms)",Summary=sprintf("%.01f ± %.01f, (N = %d)", mean(duration)*1000, sd(duration)*1000, .N))],
    .SD[quality==1 & glissade_fast_ids!=0,list(duration=.SD[.N,time]-.SD[1,time]),by=glissade_fast_ids][,list(Measure="Glissade-fast dur. (ms)",Summary=sprintf("%.01f ± %.01f, (N = %d)", mean(duration)*1000, sd(duration)*1000, .N))],
    .SD[quality==1 & fixation_ids!=0,list(velocity=mean(v)),by=fixation_ids][,list(Measure="Fixation vel. (°/s)",Summary=sprintf("%.01f ± %.01f", mean(velocity), sd(velocity)))],
    .SD[quality==1 & saccade_ids!=0,list(velocity=max(v)),by=saccade_ids][,list(Measure="Saccade peak vel. (°/s)",Summary=sprintf("%.01f ± %.01f", mean(velocity), sd(velocity)))],
    .SD[quality==1 & saccade_ids!=0,list(acceleration=max(a)),by=saccade_ids][,list(Measure="Saccade peak acc. (°/s²)",Summary=sprintf("%.01f ± %.01f", mean(acceleration), sd(acceleration)))],
    .SD[,list(Measure="Max peak vel. (°/s)",Summary=sprintf("%.01f", max(v)))],
    .SD[,list(Measure="Max peak acc (°/s)",Summary=sprintf("%.01f", max(a)))],
    .SD[,list(Measure="% glissadic saccades",Summary=sprintf("%.01f", 100*((max(glissade_slow_ids)+max(glissade_fast_ids))/max(saccade_ids))))],
    .SD[,list(blinks=.SD[(class=="Blink"),.N]/.N)][,list(Measure="% blinks",Summary=sprintf("%.01f", 100*blinks))],
    .SD[,list(noise=.SD[(class=="Noise"),.N]/.N)][,list(Measure="% noise",Summary=sprintf("%.01f", 100*noise))]
  )),by=by]
}

#' @export
gazetools$methods(summary = function(filter, by=NULL) {
  .call <- as.call(quote(.self$data[]))
  .call[[3]] <- match.call()$filter
  summary.gazetools(eval(.call), by=by)
})
