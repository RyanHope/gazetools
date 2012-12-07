setClass("classify", 
         representation(fixation_ids = "numeric", saccade_ids = "numeric",
                        algorithm="character", thresholds="numeric"),
         contains="character")

setMethod("as.data.frame", signature(x = "classify", row.names = "missing", optional = "missing"),
          function(x) {
            data.frame(class = x@.Data, fixation_ids = x@fixation_ids, saccade_ids = x@saccade_ids)
          }
)