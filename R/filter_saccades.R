filter_saccades <- function(class, samplerate, min.saccade.duration = 20) {
  n <- min.saccade.duration / (1000/samplerate)
  for (id in unique(class@saccade_ids)) {
    ids <- which(class@saccade_ids == id)
    if (id > 0 & length(ids) < n) {
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