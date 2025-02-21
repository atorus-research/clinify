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
  lapply(page_nums, \(i) list(rows = rows[[i]], cols = cols[[i]]))
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
}

group_by_ <- function(refdat, group_by) {
  browser()
  # Find every index the page by variable changes
  splits <- refdat[[group_by]] == c(NA, head(refdat[[group_by]], -1))
  splits[1] <- FALSE # Easy indexing of 1 which is NA
  group_starts <- which(!splits)
  group_ends <- c((group_starts - 1)[-1], nrow(refdat))

  groups <- make_page_vecs(group_starts, group_ends)
  names(groups) <- refdat[[group_by]][group_starts]
  groups
}

page_by_ <- function(refdat, page_by) {
  # Find every index the page by variable changes
  splits <- refdat[[page_by]] == c(NA, head(refdat[[page_by]], -1))
  splits[1] <- FALSE # Easy indexing of 1 which is NA
  page_starts <- which(!splits)
  page_ends <- c((page_starts - 1)[-1], nrow(refdat))
  make_page_vecs(page_starts, page_ends)
}

max_rows_ <- function(refdat, max_rows) {
  tot_rows <- nrow(refdat)
  page_ends <- which(seq_along(1 : tot_rows) %% max_rows == 0)
  page_starts <- page_ends - max_rows + 1

  if (tot_rows %% max_rows) {
      page_starts[length(page_starts) + 1] <- tail(page_ends, 1) + 1
      page_ends[length(page_ends) + 1] <- tot_rows
  }
  # Create vectors for each row that belongs to a page
  make_page_vecs(page_starts, page_ends)
}

prep_pagination_ <- function(x) {
  config <- x$clinify_config
  refdat <- x$body$dataset
  
  col_vecs <- NULL
  page_vecs <- NULL



  # Establish page by first because page var will strip out of clintable
  if (!is.null(config$page_by)) {
    # Slice the order variable out of the clintable
    if (config$page_by %in% x$col_keys) {
      key_idx <- eval_select(config$page_by, refdat)
      cols <- seq(1:ncol(refdat))[-key_idx]
      x <- slice_clintable(x, 1:nrow(refdat), cols)
    }
    page_vecs <- page_by_(refdat, config$page_by)
  } 

  # Make Column vectors
  if (!is.null(config$key_cols) & !is.null(config$col_groups)) {
    # Alternating pages
    col_vecs <- alt_pages_(refdat, config$key_cols, config$col_groups)
    if (is.null(config$page_by) && is.null(config$max_rows)) {
      message("NOTE: Alternating pages were set, but no selection for row wise pagination was configured",
              " Defaulting to 20 rows per page.")
      config$max_rows <- 20
    }
  } else {
    # Defaults
    col_vecs <- list(eval_select(x$col_keys, refdat))
  }

  # Make page vectors - catch this last because it could be set during alt pages
  if (!is.null(config$max_rows)) {
    page_vecs <- max_rows_(refdat, config$max_rows)
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