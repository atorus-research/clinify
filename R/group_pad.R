clin_group_pad <- function(x, pad_by, size = 9) {
  refdat <- x$body$dataset

  # Find every index the page by variable changes
  splits <- refdat[[pad_by]] == c(NA, head(refdat[[pad_by]], -1))
  splits[1] <- TRUE # Easy indexing of 1 which is NA
  which(!splits)
  padding(x, i = which(!splits), padding.top = size + 5)
}
