#' Configure alternating pages during pagination of a clintable
#'
#' This function configures alternating pages on a clintable object.
#' @param x A clintable object
#' @param key_cols A character vector of variable names
#' @param col_groups A list of character vectors of variable names
#' @param byvar A variable in the input dataset to use for pagination settings
#' @param max_rows The maximum number of rows to include on a page
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' ct <- clintable(mtcars)
#'
#' ct <- clin_alt_pages(
#'   ct,
#'   key_cols = c('mpg', 'cyl', 'hp'),
#'   col_groups = list(
#'     c('disp', 'drat', 'wt'),
#'     c('qsec', 'vs', 'am'),
#'     c('gear', 'carb')
#'   ),
#'   max_rows = 10
#' )
clin_alt_pages <- function(x, key_cols, col_groups) {
  x$clinify_config$pagination_method <- "custom"
  x$clinify_config$key_cols <- key_cols
  x$clinify_config$col_groups <- col_groups
  x
}

#' Internal method to take a vector of start and end indices and generate the
#' sequence of numbers between each point
#'
#' @param starts from indices
#' @param ends to indices
#'
#' @noRd
make_page_vecs <- function(starts, ends) {
  out <- vector('list', length = length(starts))
  for (i in seq_along(starts)) {
      out[[i]] <- starts[i]:ends[i]
  }

  # If there were names, pull them over
  if (!is.null(names(starts))) {
    names(out) <- names(starts)
  }
  out
}

#' Generate the rows/columns pairing for each output page
#'
#' @param col_vecs list of numeric vectors
#' @param page_vecs list of numeric vectors
#'
#' @return list of n pages with rows and cols elements
#'
#' @noRd
make_ind_list <- function(col_vecs, page_vecs) {

  page_nums <- seq(1, length(col_vecs) * length(page_vecs))
  rows <- unlist(
      lapply(page_vecs, \(x) rep(list(x), length(col_vecs))),
      recursive = FALSE
  )

  cols <- rep(col_vecs, length(page_vecs))
  if (!is.null(names(page_vecs))) {
    nms <- unlist(
      lapply(names(page_vecs), \(x) rep(list(x), length(col_vecs))),
      recursive = FALSE
    )

    out <- lapply(page_nums, \(i) list(rows = rows[[i]], cols = cols[[i]], label = nms[[i]]))
  }
  else {
    out <- lapply(page_nums, \(i) list(rows = rows[[i]], cols = cols[[i]]))
  }
  out
}


#' Configure pagination using a page variable
#' 
#' @param x A clintable object
#' @param byvar A column in the table used for page grouping
#'
#' @return A clintable object
#' @export
#'
#' @examples
clin_page_by <- function(x, page_by, max_rows=10) {
  if (!missing(page_by)) {
    x$clinify_config$page_by <- page_by
  } else {
    x$clinify_config$max_rows <- max_rows
  }
  x
}

#' Configure a clintable to table by a grouping variable, which will be used as a label
#'
#' @return A clintable object
#' @export
#'
#' @examples
clin_group_by <- function(x, group_by) {
  x$clinify_config$group_by <- group_by
  x
}

group_by_ <- function(refdat, group_by) {
  # Find every index the page by variable changes
  splits <- refdat[[group_by]] == c(NA, head(refdat[[group_by]], -1))
  splits[1] <- FALSE # Easy indexing of 1 which is NA
  group_starts <- which(!splits)
  
  names(group_starts) <- refdat[[group_by]][group_starts]
  group_starts
}

page_by_ <- function(refdat, page_by, group_by=NULL) {

  if (!is.null(page_by)) {
    # Find every index the page by variable changes
    splits <- refdat[[page_by]] == c(NA, head(refdat[[page_by]], -1))
    splits[1] <- FALSE # Easy indexing of 1 which is NA

    page_starts <- which(!splits)
  } else {
    page_starts <- numeric()
  }

  if (!is.null(group_by)) {
    gs <- group_by_(refdat, group_by)
    page_starts <- sort(union(page_starts, gs))
    # Bring the group names over and carry them forward
    names(page_starts)[which(page_starts %in% gs)] <- names(gs)
    names(page_starts) <- zoo::na.locf(names(page_starts))
  }
  
  page_ends <- unname(c((page_starts - 1)[-1], nrow(refdat)))
  make_page_vecs(page_starts, page_ends)
}

max_rows_ <- function(refdat, max_rows, group_by=NULL) {
  tot_rows <- nrow(refdat)

  # Grab the group starts
  if (!is.null(group_by)) {
    gs <- group_by_(refdat, group_by)
  } else {
    gs <- 1
  }

  # Approximate the total pages so I can avoid appends
  page_starts <- integer(ceiling(tot_rows / max_rows))
  page_starts[1] <- 1
  ps <- 1L

  # Start iterating at second index until pages are done
  i <- 2
  while ((ps + max_rows) <= tot_rows) {
    # Increment page start by max rows or the group start is next in line
    ps <- min(ps + max_rows, suppressWarnings(min(gs[gs > ps])))
    page_starts[i] <- ps
    i <- i+1
  }

  # Carry the names forward
  if (!is.null(names(gs))) {
    names(page_starts)[which(page_starts %in% gs)] <- names(gs)
    names(page_starts) <- zoo::na.locf(names(page_starts))
  }

  page_ends <- unname(c((page_starts - 1)[-1], tot_rows))
  # Create vectors for each row that belongs to a page
  make_page_vecs(page_starts, page_ends)
}

prep_pagination_ <- function(x) {


  config <- x$clinify_config
  refdat <- x$body$dataset
  
  col_vecs <- NULL
  page_vecs <- NULL

  # TODO: Something off here? 
  if (!is.null(config$col_groups) && is.null(config$page_by) && is.null(config$max_rows)) {

    message("NOTE: Alternating pages were set, but no selection for row wise pagination was configured",
            " Defaulting to 20 rows per page.")
    config$max_rows <- 20
    page_vecs <- max_rows_(refdat, config$max_rows, group_by = config$group_by)

    # Establish page by and group by first because page var will strip out of clintable
  } else if (is.null(config$max_rows) && (!is.null(config$page_by) || !is.null(config$group_by))) {
    page_vecs <- page_by_(refdat, config$page_by, group_by = config$group_by)
  } 

  # Slice the page by and group by out of the clintable
  if (any(c(config$page_by, config$group_by) %in% x$col_keys)) {
    key_idx <- eval_select(c(config$page_by, config$group_by), refdat)
    cols <- seq(1:ncol(refdat))[-key_idx]
    x <- slice_clintable(x, 1:nrow(refdat), cols)
  }
  
  # Make Column vectors
  if (!is.null(config$col_groups)) {
    # Alternating pages
    col_vecs <- alt_pages_(refdat, config$key_cols, config$col_groups)
  } else {
    # Defaults
    col_vecs <- list(eval_select(x$col_keys, refdat))
  }

  x$clinify_config$pagination_idx <- make_ind_list(col_vecs, page_vecs)
  x
}


alt_pages_ <- function(refdat, key_cols, col_groups) {
  key_idx <- eval_select(key_cols, refdat)
  grp_idx <- lapply(col_groups, eval_select, data=refdat)
  # Create column vectors
  lapply(grp_idx, \(x) append(key_idx, x))
}