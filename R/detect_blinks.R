outlier <- function(x, threshold=3.5) {
  sapply(x, function(x, .mean, .mad, .threshold) {
    abs((0.6745*(x-.mean))/.mad) > .threshold
  }, .mean=mean(x), .mad=mad(x), .threshold=threshold)
}

detect_blinks <- function(pupil, samplerate, window = 51, threshold = 1000) {
  ts <- 1 / samplerate
  d <- data.frame(x=1:length(pupil),pupil=pupil)
  d$pupil_smoothed <- lowess(d$pupil,f=.001)$y
  .median <- median(d$pupil_smoothed)
  d$v <- lowess(abs(sgolayfilt(d$pupil_smoothed, n = window, p = 2, m = 1, ts = ts)),f=.01)$y
  d$blink <- d$v > threshold
  d$blink[which(d$pupil_smoothed < .median/2)] <- T
  p <- ggplot(melt(d, id.vars=c("x","blink"))) +
    geom_point(aes(x=x, y=value, color=blink)) + facet_grid(variable~.,scales="free_y")
  print(p)
  d
}

trial= 25
d <- subset(read.table(sprintf("~/williams-data/WilliamsSearch_2012-11-12_17-07-38_16af6d5c/trial-%02d.txt",trial),header=T),event_type=="ET_SPL")
z=detect_blinks(d$smi_dxl, 500)
print(rle(z$blink))