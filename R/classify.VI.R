#' Gaze Data Classification (velocity-iterative)
#'
#' Classifies gaze data into saccades and fixations using an iteratively determined velocity threshold.
#' 
#' @template v
#' @param sigma number of standard deviations used for upper saftey margin for peak velocity threshold, 
#' saccade velocity threshold is half this value, this parameter needs to be tuned for each eyetracker/dataset combination
#' @param samplerate the samplerate in Hz of the eyetracker
#' @param min.fix minimum fixations duration (in seconds)
#' @param min.sac minimum saccade duration (in seconds)
#' @template blinks
#' @param a weight of saccade onset threshold for determining saccade offset threshold
#' @param b weight of noise threshold for determining saccade offset threshold
#' 
#' @return an object of class \code{\link[=classify-class]{classify}}
#'
#' @export
#' 
#' @family classify
#' 
#' @encoding latin1
#' @references Nyström, M., & Holmqvist, K. (2010). An adaptive algorithm for fixation, saccade, and glissade detection in eyetracking data. Behavior Research Methods, 42(1), 188-204.
#'
#' @examples
#' # Classification ignorning blinks
#' data(smi)
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl))
#' d.c <- classify.VI(d.pva@@v)
#' str(d.c)
#' 
#' # Classification accounting for blinks
#' d.pva <- with(smi, pva(smi_sxl, smi_syl, 
#'                        500, 1680, 1050, 473.76, 296.1, 
#'                        smi_ezl, smi_exl, smi_eyl, pupil=smi_dxl))
#' d.c <- classify.VI(d.pva@@v, blinks=d.pva@@blinks)
#' str(d.c)
#' 
classify.VI <- function(v, vt = 100, sigma = 6, samplerate = 500, min.fix = .040, min.sac = .010, blinks = FALSE, a=.7, b=(1-a))
{
  nt <- st <- vt

  v <- data.frame(v=v)
  v$blinks <- blinks

  # Calculate velocity peak threshold
  while (T) {
    f <- subset(v, blinks==FALSE & v<vt)
    vtn <- mean(f$v) + sigma * sd(f$v)
    if (abs(vtn-vt)<1)
      break
    vt <- vtn
  }

  # Calclulate saccade onset threshold
  while (T) {
    f <- subset(v, blinks==FALSE & v<st)
    stn <- mean(f$v) + (sigma/2) * sd(f$v)
    if (abs(stn-st)<1)
      break
    st <- stn
  }

  offset <- min.fix/(1/samplerate)
  m <- nrow(v)
  class <- rep("FIXATION", m)
  vranges <- find_peak_ranges(v$v, vt)
  if (!is.null(vranges) && nrow(vranges)>0) {
    for (i in 1:nrow(vranges)) {
      rmin <- vranges[i,1]
      while (v[rmin,"v"]>st && (rmin-1)>1 && length(v[rmin-1,"v"])==1)
        rmin <- rmin-1
      while ((rmin-1)>1 && length(v[rmin-1,"v"])==1 && v[rmin-1,"v"]<v[rmin,"v"])
        rmin <- rmin-1

      # calculate saccade offset threshold
      if (rmin-offset < 1) next
      vtmp <- v[(rmin-offset):rmin,]
      z <- NULL
      while (T) {
        f <- subset(vtmp, blinks==FALSE & v<nt)
        if (nrow(f)==0) break
        z <- mean(f$v)
        ntn <- z + (sigma/2) * sd(f$v)
        if (is.na(ntn) || abs(ntn-nt)<1)
          break
        nt <- ntn
      }

      st_off <- a*st + b*nt
      rmax <- vranges[i,2]
      while (v[rmax,"v"]>st_off && (rmax+1)<m && length(v[rmax+1,"v"])==1) {
        rmax <- rmax+1
      }
      while ((rmax+1)<m && length(v[rmax+1,"v"])==1 && v[rmax+1,"v"]<v[rmax,"v"])
        rmax <- rmax+1

      if (rmax-rmin<min.sac) next
      if (!is.null(z) && z>vt) next
      class[rmin:rmax] <- "SACCADE"

      # detect fast-glissades
      fast <- FALSE
      peak <- FALSE
      for (i in 0:(offset-1)) {
        if (!peak && !is.na(v[(rmax+i),"v"]) && v[(rmax+i),"v"]>vt) {
          peak <- TRUE
          next
        } else {
          vn <- v[(rmax+i),"v"]
          if (peak && !is.na(vn) && vn<=vt) {
            fast <- TRUE
            while ((rmax+i+1)<m && length(v[rmax+i+1,"v"])==1 && v[rmax+i+1,"v"]<v[rmax+i,"v"])
              i <- i + 1
            class[rmax:(rmax+(i-1))] <- "GLISSADE"
            break
          }
        }
      }
      if (!fast) {
        peak <- FALSE
        # detect slow-glissades
        for (i in 0:(offset-1)) {
          if (!peak && !is.na(v[(rmax+i),"v"]) && v[(rmax+i),"v"]>st) {
            peak <- TRUE
            next
          } else {
            vn <- v[(rmax+i),"v"]
            if (peak && !is.na(vn) && vn<=st) {
              while ((rmax+i+1)<m && length(v[rmax+i+1,"v"])==1 && v[rmax+i+1,"v"]<v[rmax+i,"v"])
                i <- i + 1
              class[rmax:(rmax+(i-1))] <- "GLISSADE"
              break
            }
          }
        }
      }
    }
  }

  fixation_ids <- event_ids(class, "FIXATION")
  saccade_ids <- event_ids(class, "SACCADE")
  glissade_ids <- event_ids(class, "GLISSADE")
  if (!is.null(blinks)) {
    class[blinks] <- "BLINK"
    fixation_ids[blinks] <- 0
    saccade_ids[blinks] <- 0
    glissade_ids[blinks] <- 0
  }

  new("classify", class,
      fixation_ids = fixation_ids,
      saccade_ids = saccade_ids,
      glissade_ids = glissade_ids,
      algorithm = "velocity-iterative", thresholds = list(vt=vt,st=st,nt=nt))
}