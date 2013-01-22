filter_saccades <- function(dpva, class, samplerate, min.saccade.duration = 20, min.saccade.amp = 1.5, verbose=T) {
  distance <- function(x1,y1,x2,y2) sqrt((x2-x1)**2+(y2-y1)**2)
  n <- min.saccade.duration / (1000/samplerate)
  ids <- unique(class@saccade_ids)
  ids <- ids[ids>0]
  if (class@fixation_ids[1]==0)
    ids <- ids[2:length(ids)]
  for (id in ids) {
    ids <- which(class@saccade_ids == id)
    if (length(ids) < n) {
      if (verbose)
        print(sprintf("Dropping saccade id %d, length %d ms", id, length(ids)*(1000/samplerate)))
      class@saccade_ids[ids] <- 0
      fix <- class@fixation_ids[min(ids)-1]
      class@fixation_ids[ids] <- fix
      class@fixation_ids[which(class@fixation_ids==(fix+1))] <- fix
      class@.Data[ids] <- "FIXATION"
    }
  }
  ids <- unique(class@fixation_ids)
  ids <- ids[ids>0]
  i <- 2
  if (min.saccade.amp>0) {
    while (i <= length(ids)) {
      ids2 <- which(class@fixation_ids == ids[i])
      ids1 <- which(class@fixation_ids == ids[i-1])
      dist <- subtendedAngle(mean(dpva@sx[ids2]),mean(dpva@sy[ids2]),mean(dpva@sx[ids1]),mean(dpva@sy[ids1]), 
                             1680, 1050, 473.76, 296.1, mean(dpva@ez[c(ids1,ids2)]))
      if (dist < min.saccade.amp) {
        if (verbose)
          print(sprintf("Merging fixation id %d, distance from previous fixation %f deg/va", ids[i], dist))
        class@fixation_ids[ids2] <- ids[i-1]
        ids3 <- (tail(ids1,1)+1):(head(ids2,1)-1)
        class@.Data[ids3] <- "FIXATION"
        class@saccade_ids[ids3] <- 0
        ids[i] = ids[i-1]
      }
      i <- i + 1
    }
  }
  fixruns <- rle(class@fixation_ids)$values
  for(i in 1:length(fixruns)) {
    if (i > 1 && fixruns[i] != 0 && fixruns[i-1] != 0)
      class@fixation_ids[which(class@fixation_ids == fixruns[i])] <- fixruns[i-1]
  }
  class
}