clin_col_widths <- function(x, ...) {

  stopifnot(inherits(x, "clintable"))
    
  wd <- clin_default_table_width()    
  args <- list(...)
  if (!all(vapply(args, is.numeric, TRUE))) {
    stop("All width arguments must be numeric")
  }
  cw <- unlist(args)

  if (any(cw > 1)) {
    stop("Width arguments represent present width of page and cannot be >1")
  }
   
  # Make sure the cols exist
  dne <- setdiff(names(cw), x$col_keys)
  if (length(dne) >= 1) {
    stop(sprintf(
      "The following columns are not present in the clintable:\n%s", 
      paste(dne, collapse=", ")
    ))
  }

  leftovers <- setdiff(x$col_keys, names(cw))
  x <- width(x, j=names(cw), cw * wd)
  if (length(leftovers) > 0) x <- width(x, j=leftovers, (wd - sum(cw))/wd)
  x
}