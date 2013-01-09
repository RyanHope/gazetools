filter_saccades <- function(class, samplerate, min.saccade.duration = 20, verbose=F) {
  n <- min.saccade.duration / (1000/samplerate)
  ids <- unique(class@saccade_ids)
  if (class@fixation_ids[1]==0)
    ids <- ids[2:length(ids)]
  for (id in ids) {
    ids <- which(class@saccade_ids == id)
    if (id > 0 & length(ids) < n) {
      if (verbose)
        print(sprintf("Dropping saccade id %d, length %d ms", id, length(ids)*(1000/samplerate)))
      class@saccade_ids[ids] <- 0
      fix <- class@fixation_ids[min(ids)-1]
      class@fixation_ids[ids] <- fix
      class@fixation_ids[which(class@fixation_ids==(fix+1))] <- fix
      class@.Data[ids] <- "FIXATION"
    }
  }
  class
}