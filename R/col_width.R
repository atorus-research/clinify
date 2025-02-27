clin_col_widths <- function(x, ...) {

  stopifnot(inherits(x, "clintable"))
    
  wd <- clin_default_table_width()  
  
  # Pull out the column widths
  args <- list(...)
  if (!all(vapply(args, is.numeric, TRUE))) {
    stop("All width arguments must be numeric")
  }
  cw <- unlist(args)

  if (any(cw > 1)) {
    stop("Width arguments represent percent width of page. and cannot be >1")
  }
   
  # Make sure the cols exist
  dne <- setdiff(names(cw), x$col_keys)
  if (length(dne) >= 1) {
    stop(sprintf(
      "The following columns are not present in the clintable:\n%s", 
      paste(dne, collapse=", ")
    ))
  }


  # Handle alternating cols
  if (!is.null(x$clinify_config$col_groups)) {
    col_groups <- x$clinify_config$col_groups
    key_cols <- x$clinify_config$key_cols
    # Expand the total width to the length of the sub groups
    key_col_wd <- cw[names(cw) %in% key_cols]

    leftover_keys <- setdiff(key_cols, names(cw))
    lo_keys <- NULL

    # Set the widths for 
    for (i in seq_along(col_groups)) {
      cg_wd <- cw[names(cw) %in% col_groups[[i]]]
      if (sum(c(key_col_wd, cg_wd)) > 1) {
        stop("Key columns + alternating page column widths sum to >1. Width arguments represent percent width of page.")
      }
      x <- width(x, j=names(cg_wd), cg_wd * wd)

      leftovers <- setdiff(c(col_groups[[i]]), names(cw))

      if (length(leftovers) > 0) {
        tot_pg_pct <- sum(key_col_wd, cg_wd)

        # Set the leftover key columns based on the first page and retain that length
        if (is.null(lo_keys) && length(leftover_keys) > 0) {
          lo_wd_pct <- (1-tot_pg_pct) / (length(leftovers) + length(leftover_keys))
          lo_wd <- lo_wd_pct * wd

          lo_keys <- rep(lo_wd_pct, length(leftover_keys))
          names(lo_keys) <- leftover_keys
          x <- width(x, j=leftover_keys, lo_keys)
          key_col_wd <- c(key_col_wd, lo_keys)
        } else {
          lo_wd <- ((1-tot_pg_pct) / length(leftovers)) * wd
        }
        
        x <- width(x, j=leftovers, lo_wd)
      }

    } 
    # Set the key col widths
    x <- width(x, j=names(key_col_wd), key_col_wd * wd)
  } else {
    # Standard table
    if (sum(cw) > 1) {
      stop("Columns widths sum to >1. Width arguments represent percent width of page.")
    }

    leftovers <- setdiff(x$col_keys, names(cw))
    x <- width(x, j=names(cw), cw * wd)
    lo_wd <- ((1 - sum(cw))/length(leftovers)) * wd
    if (length(leftovers) > 0) x <- width(x, j=leftovers, lo_wd)
  }
  x
}