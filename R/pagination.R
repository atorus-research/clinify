#' Configure alternating pages during pagination of a clintable
#'
#' This function configures alternating pages on a clintable object.
#' @param x A clintable object
#' @param key_cols A character vector of variable names
#' @param col_groups A list of character vectors of variable names
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' ct <- clintable(mtcars)
#'
#' clin_alt_pages(
#'   ct,
#'   key_cols = c("mpg", "cyl", "hp"),
#'   col_groups = list(
#'     c("disp", "drat", "wt"),
#'     c("qsec", "vs", "am"),
#'     c("gear", "carb")
#'   )
#' )
clin_alt_pages <- function(x, key_cols, col_groups) {
  x$clinify_config$pagination_method <- "custom"
  x$clinify_config$key_cols <- key_cols
  x$clinify_config$col_groups <- col_groups
  x
}

#' Configure pagination using a page variable
#'
#' @param x A clintable object
#' @param page_by A variable in the input dataframe to use for pagination
#' @param max_rows If no page_by, the maximum rows allowed per page
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' dat <- mtcars
#' dat["page"] <- c(
#'   rep(1, 10),
#'   rep(2, 10),
#'   rep(3, 10),
#'   c(4, 4)
#' )
#'
#' clintable(dat) |>
#'   clin_page_by("page")
#'
#' clintable(mtcars) |>
#'   clin_page_by(max_rows = 10)
clin_page_by <- function(x, page_by, max_rows = 10) {
  x$clinify_config$pagination_method <- "custom"

  if (!missing(page_by)) {
    x$clinify_config$page_by <- page_by
  } else {
    x$clinify_config$max_rows <- max_rows
  }
  x
}

#' Configure a clintable to table by a grouping variable, which will be used as a label
#'
#' @param x A clintable object
#' @param group_by A character vector of variable names which will be used for grouping and attached
#'   as a label above the table headers
#' @param caption_by A single element string of a variable name which will be used as a caption
#'   attached below the table body and above in the footer. Defaults to NULL.
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' clintable(iris) |>
#'   clin_group_by("Species")
clin_group_by <- function(x, group_by, caption_by = NULL) {
  x$clinify_config$pagination_method <- "custom"
  x$clinify_config$group_by <- group_by
  x$clinify_config$caption_by <- caption_by
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
  out <- vector("list", length = length(starts))
  for (i in seq_along(starts)) {
    out[[i]] <- starts[i]:ends[i]
  }

  # If there were names, pull them over
  if (!is.null(attr(starts, "group_labels"))) {
    attr(out, "group_labels") <- attr(starts, "group_labels")
  }
  if (!is.null(attr(starts, "captions"))) {
    attr(out, "captions") <- attr(starts, "captions")
  }
  out
}

#' Generate the rows/columns pairing for each output page
#'
#' If the page vectors are named, the name will be applied
#' to the label element of each list
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

  nms <- NULL
  caps <- NULL

  if (!is.null(attr(page_vecs, "group_labels"))) {
    nms <- unlist(
      apply(
        attr(page_vecs, "group_labels"),
        1,
        \(x) rep(list(as.character(x)), length(col_vecs))
      ),
      recursive = FALSE
    )
  }

  if (!is.null(attr(page_vecs, "captions"))) {
    caps <- unlist(
      apply(
        attr(page_vecs, "captions"),
        1,
        \(x) rep(list(as.character(x)), length(col_vecs))
      ),
      recursive = FALSE
    )
  }

  out <- lapply(page_nums, \(i) {
    list(
      rows = rows[[i]],
      cols = cols[[i]],
      label = nms[[i]],
      captions = caps[[i]]
    )
  })

  out
}

#'  Get indices of the beginning of each group based on input variable
#'
#' @param refdat The dataframe stored in the flextable object
#' @param group_by A character string of the variable from refdat to be used
#'
#' @return Group start indices
#' @noRd
group_by_ <- function(refdat, group_by, caption_by = NULL) {
  # Mock a matrix to catch each split of the group by
  splits <- matrix(
    TRUE,
    nrow = nrow(refdat),
    ncol = length(c(group_by, caption_by)),
    dimnames = list(NULL, c(group_by, caption_by))
  )

  # For each group in the splits,
  for (g in c(group_by, caption_by)) {
    splits[, g] <- refdat[[g]] == c(NA, head(refdat[[g]], -1))
    splits[1, g] <- FALSE
  }

  group_starts <- which(!apply(splits, 1, all))
  group_vals <- as.matrix(
    refdat[group_starts, c(group_by, caption_by)],
    dimnames = list(NULL, c(group_by, caption_by))
  )

  rownames(group_vals) <- NULL # I hate rownames
  colnames(group_vals) <- c(group_by, caption_by)

  list(group_starts = group_starts, group_vals = group_vals)
}

#' Generate page vectors using page_by variable with group considerations
#'
#' @param refdat The dataframe stored in the flextable object
#' @param max_rows A variable in refdat used to determine pages
#' @param group_by A character string of the variable from refdat to be used
#'
#' @return Page vectors
#' @noRd
page_by_ <- function(refdat, page_by, group_by = NULL, caption_by = NULL) {
  if (!is.null(page_by)) {
    # Find every index the page by variable changes
    splits <- refdat[[page_by]] == c(NA, head(refdat[[page_by]], -1))
    splits[1] <- FALSE # Easy indexing of 1 which is NA

    page_starts <- which(!splits)
  } else {
    page_starts <- numeric()
  }

  if (!is.null(group_by)) {
    gs <- group_by_(refdat, group_by, caption_by)
    page_starts <- sort(union(page_starts, gs$group_starts))
    group_labels <- matrix(
      NA_character_,
      nrow = length(page_starts),
      ncol = length(group_by)
    )
    captions <- matrix(
      NA_character_,
      nrow = length(page_starts),
      ncol = length(caption_by)
    )

    # Bring the group names over and carry them forward
    group_labels[which(page_starts %in% gs$group_starts), ] <- gs$group_vals[,
      group_by
    ]
    captions[which(page_starts %in% gs$group_starts), ] <- gs$group_vals[,
      caption_by
    ]
    attr(page_starts, "group_labels") <- zoo::na.locf(group_labels)
    attr(page_starts, "captions") <- zoo::na.locf(captions)
  }

  page_ends <- unname(c((page_starts - 1)[-1], nrow(refdat)))
  make_page_vecs(page_starts, page_ends)
}

#' Generate page vectors by maximum rows with group considerations
#'
#' @param refdat The dataframe stored in the flextable object
#' @param max_rows The max rows per page
#' @param group_by A character string of the variable from refdat to be used
#'
#' @return Page vectors
#' @noRd
max_rows_ <- function(refdat, max_rows, group_by = NULL, caption_by = NULL) {
  tot_rows <- nrow(refdat)

  # Grab the group starts
  if (!is.null(group_by)) {
    gs <- group_by_(refdat, c(group_by, caption_by))
  } else {
    gs <- list(group_starts = 1)
  }

  # Approximate the total pages so I can avoid appends
  page_starts <- integer(max(
    ceiling(tot_rows / max_rows),
    length(gs$group_starts)
  ))
  page_starts[1] <- 1
  ps <- 1L

  # Start iterating at second index until pages are done
  i <- 2
  while (
    (ps + max_rows) <= tot_rows ||
      (!is.na(gs$group_starts[i]) && (gs$group_starts[i] < tot_rows))
  ) {
    # Increment page start by max rows or the group start is next in line
    ps <- min(
      ps + max_rows,
      suppressWarnings(min(gs$group_starts[gs$group_starts > ps]))
    )
    page_starts[i] <- ps
    i <- i + 1
  }

  # Carry the names forward
  if (!is.null(group_by)) {
    group_labels <- matrix(
      NA_character_,
      nrow = length(page_starts),
      ncol = length(group_by),
      dimnames = list(NULL, group_by)
    )
    # Bring the group names over and carry them forward
    group_labels[which(page_starts %in% gs$group_starts), ] <- gs$group_vals[,
      group_by
    ]
    attr(page_starts, "group_labels") <- zoo::na.locf(group_labels)
  }
  if (!is.null(caption_by)) {
    captions <- matrix(
      NA_character_,
      nrow = length(page_starts),
      ncol = length(caption_by),
      dimnames = list(NULL, caption_by)
    )
    captions[which(page_starts %in% gs$group_starts), ] <- gs$group_vals[,
      caption_by
    ]
    attr(page_starts, "captions") <- zoo::na.locf(captions)
  }

  page_ends <- unname(c((page_starts - 1)[-1], tot_rows))
  # Create vectors for each row that belongs to a page
  make_page_vecs(page_starts, page_ends)
}

#' Generate alternating page column vectors
#'
#' @param refdat The dataframe stored in the flextable object
#' @param key_cols A character vector of columns to apply on each page
#' @param col_groups A character vector of variable that will appear on individual pages
#'
#' @return Page vectors
#' @noRd
alt_pages_ <- function(refdat, key_cols, col_groups) {
  key_idx <- eval_select(key_cols, refdat)
  grp_idx <- lapply(col_groups, eval_select, data = refdat)
  # Create column vectors
  lapply(grp_idx, \(x) append(key_idx, x))
}

#' This is the driver function to process the pagination calculations. Based
#' on the settings applied through object creation, this function will interpret
#' and create the indices necessary for pagination to be applied
#'
#' @param x A clintable object
#'
#' @return A clintable with the pagination IDs populated
#' @noRd
prep_pagination_ <- function(x) {
  config <- x$clinify_config
  refdat <- x$body$dataset

  col_vecs <- NULL
  page_vecs <- NULL

  # If alternating and no paging set, apply max rows
  if (
    !is.null(config$col_groups) &&
      is.null(config$page_by) &&
      is.null(config$max_rows)
  ) {
    message(
      "NOTE: Alternating pages were set, but no selection for row wise pagination was configured",
      " Defaulting to 20 rows per page."
    )
    config$max_rows <- 20
    page_vecs <- max_rows_(
      refdat,
      config$max_rows,
      group_by = config$group_by,
      caption_by = config$caption_by
    )
    # Establish page by and group by first because page var will strip out of clintable
  } else if (
    is.null(config$max_rows) &&
      (!is.null(config$page_by) || !is.null(config$group_by))
  ) {
    page_vecs <- page_by_(
      refdat,
      config$page_by,
      group_by = config$group_by,
      caption_by = config$caption_by
    )
  } else if (!is.null(config$max_rows)) {
    page_vecs <- max_rows_(
      refdat,
      config$max_rows,
      group_by = config$group_by,
      caption_by = config$caption_by
    )
  }

  # Slice any unnecessary vars out of the clintable
  if (
    any(c(config$page_by, config$group_by, config$caption_by) %in% x$col_keys)
  ) {
    key_idx <- eval_select(
      c(config$page_by, config$group_by, config$caption_by),
      refdat
    )
    cols <- seq(1:ncol(refdat))[-key_idx]
    x <- slice_clintable(x, 1:nrow(refdat), cols, skip_spans = TRUE)
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

#' Enable Word Auto-Pagination Using Group Variable
#'
#' This function uses the applies the functionality `flextable::keep_with_next()`
#' by automatically building the row indices using some grouping variable. Each group
#' identified by the variable (i.e. when the value of the variable changes) will be set
#' as a "keep_with_next" group in Word. Using this functionality, Word will attempt not to
#' break that group across pages, enabling smoother pagination without having to do specific
#' calculations of page breaks.
#'
#' @param x A clintable object
#' @param group_var A string containing a variable name of the input dataset used to
#'   calculate groups
#'
#' @return A clintable object
#' @export
#'
#' @examples
#'
#' clintable(mtcars) |>
#'   clin_auto_page("gear")
#'
clin_auto_page <- function(x, group_var) {
  x$clinify_config$auto_page_var <- group_var
  x
}

#' Identify the group indices of a variable and flag keep_with_next
#'
#' This leverages Word's keep_with_next functionality to make sure that variables
#' in the specified indices don't insert page breaks in between
#'
#' @param x the clintable object
#'
#' @return A clintable object
#' @noRd
auto_page_ <- function(x) {
  group_var <- x$clinify_config$auto_page_var
  refdat <- x$body$dataset
  refvar <- refdat[, group_var]

  dont_keep <- which(refvar[-length(refvar)] != refvar[-1])
  keep <- (1:length(refvar))[-dont_keep]

  x <- keep_with_next(x, i = keep)
  x
}
