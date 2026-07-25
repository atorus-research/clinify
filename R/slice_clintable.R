#' Slice a clintable object
#'
#' This function takes in a clintable (or a flextable) object and slices the
#' columns based on the provided rows and columns, allowing you to effectively
#' subset a flextable object that has already had styling applied, chunking it
#' as desired.
#'
#' @param x A clintable or flextable object
#' @param rows Subset of rows to extract
#' @param columns Subset of columns to extract
#' @param reapply_config Carry the clinify config to the sliced page
#'
#' @return A clinpage object
#'
#' @examples
#' x <- flextable::flextable(mtcars)
#' y <- slice_clintable(x, 20:32, c(1:3, 5:8))
#' z <- flextable::flextable(mtcars[20:32, c(1:3, 5:8)])
#' @noRd
slice_clintable <- function(
  x,
  rows,
  columns,
  reapply_config = FALSE
) {
  out <- new_clinpage()

  if (reapply_config) {
    out$clinify_config <- x$clinify_config
    class(out) <- c("clintable", "flextable")
  }

  if (!is.null(names(columns))) {
    columns <- eval_select(names(columns), x$body$dataset)
  }

  out$header <- slice_complex_tabpart(
    x$header,
    1:nrow(x$header$dataset),
    columns
  )

  out$footer <- slice_complex_tabpart(
    x$footer,
    1:nrow(x$footer$dataset),
    columns
  )

  out$body <- slice_complex_tabpart(x$body, rows, columns)

  # Pull up the formatting of the very bottom row
  out$body$styles$cells <- reapply_bottom_border(
    out$body$styles$cells,
    x$body$styles$cells,
    columns
  )
  out$col_keys <- x$col_keys[columns]
  out$caption <- x$caption
  out$blanks <- x$blanks
  out$properties <- x$properties

  out
}

#' Subset the merges of a table part
#'
#' Merged cells are stored as a count on the first cell of the merge and a
#' zero on each cell it covers, so taking a subset of rows or columns out of
#' the middle of a merge leaves counts that no longer match what is there -
#' too wide, or a covered cell whose count has been dropped along with the
#' cell that carried it. Working out which merge each cell belongs to first
#' and then counting the cells that survive gives the merges the subset
#' should have.
#'
#' Horizontal merges are the ones a column subset disturbs, and vertical
#' merges are the ones a row subset disturbs.
#'
#' @param spans The `spans` element of a table part
#' @param rows Subset of rows
#' @param columns Subset of cols
#'
#' @return A `spans` list holding the merges of the subset
#'
#' @noRd
slice_spans <- function(spans, rows, columns) {
  out <- list(
    rows = matrix(1L, nrow = length(rows), ncol = length(columns)),
    columns = matrix(1L, nrow = length(rows), ncol = length(columns))
  )

  # Carry the numeric type of the source table forward
  storage.mode(out$rows) <- storage.mode(spans$rows)
  storage.mode(out$columns) <- storage.mode(spans$columns)

  for (i in seq_along(rows)) {
    out$rows[i, ] <- slice_span_vec(spans$rows[rows[i], ], columns)
  }

  for (j in seq_along(columns)) {
    out$columns[, j] <- slice_span_vec(spans$columns[, columns[j]], rows)
  }

  out
}

#' Subset one row or column of merges
#'
#' @param v Merge counts along one row or column of a table part
#' @param keep The positions being kept
#'
#' @return The merge counts of the kept positions
#'
#' @examples
#' # A blank pair of cells followed by a spanner over four
#' slice_span_vec(c(2, 0, 4, 0, 0, 0), c(1, 2, 3)) # 2 0 1
#' @noRd
slice_span_vec <- function(v, keep) {
  if (!length(keep)) {
    return(v[0])
  }

  # Each count opens a merge and each zero continues the one before it, so
  # the position of the count identifies the merge every cell belongs to
  blocks <- cummax(seq_along(v) * (v > 0))

  # Count the cells of each merge that survived, and put the count back on
  # the first of them
  kept <- rle(blocks[keep])$lengths
  unlist(
    lapply(kept, \(n) c(n, rep(0L, n - 1L))),
    use.names = FALSE
  )
}

#' Slice a complex_tabpart object
#'
#' These are the "table like" elements, including the header,
#' body, and footer
#'
#' @param x Base table being sliced
#' @param rows Subset of rows
#' @param columns Subset of cols
#'
#' @return A clinpage object
#' @noRd
slice_complex_tabpart <- function(x, rows, columns) {
  if (nrow(x$dataset) == 0) {
    rows <- numeric()
  }

  dataset <- x$dataset[rows, columns, drop = FALSE]

  # Content element
  content <- slice_chunkset_struct(x$content, rows, columns)

  col_keys <- x$col_keys[columns]
  colwidths <- x$colwidths[columns]
  rowheights <- x$rowheights[rows]
  hrule <- x$hrule[rows]

  # Spans
  spans <- slice_spans(x$spans, rows, columns)

  # Styles
  styles <- list(cells = NULL, pars = NULL, text = NULL)

  styles$cells <- lapply(
    x$styles$cells,
    slice_fpstruct,
    rows = rows,
    columns = columns
  )
  styles$pars <- lapply(
    x$styles$pars,
    slice_fpstruct,
    rows = rows,
    columns = columns
  )
  styles$text <- lapply(
    x$styles$text,
    slice_fpstruct,
    rows = rows,
    columns = columns
  )

  # Preserve classes
  class(styles$cells) <- class(x$styles$cells)
  class(styles$pars) <- class(x$styles$pars)
  class(styles$text) <- class(x$styles$text)

  structure(
    list(
      dataset = dataset,
      content = content,
      col_keys = col_keys,
      colwidths = colwidths,
      rowheights = rowheights,
      hrule = hrule,
      spans = spans,
      styles = styles
    ),
    class = "complex_tabpart"
  )
}

#' Slice a fpstruct object
#'
#' This is a redundant data structure for many different
#' sub elements of the flextable
#'
#' @param x Base table being sliced
#' @param rows Subset of rows
#' @param columns Subset of cols
#'
#' @return clinpage object
#' @noRd
slice_fpstruct <- function(x, rows, columns) {
  out <- x
  out$keys <- out$keys[columns]
  out$nrow <- length(rows)
  out$ncol <- length(columns)
  out$data <- x$data[rows, columns, drop = FALSE]
  class(out) <- class(x)
  out
}

#' Slice a chunkset_struct object
#'
#' This is identical to fpstruct for now, but let's see
#' if differences pop up
#'
#' @param x Base table being sliced
#' @param rows Subset of rows
#' @param columns Subset of cols
#'
#' @return clinpage object
#' @noRd
slice_chunkset_struct <- function(x, rows, columns) {
  out <- x
  out$keys <- out$keys[columns]
  out$nrow <- length(rows)
  out$ncol <- length(columns)
  out$data <- x$data[rows, columns, drop = FALSE]
  class(out) <- class(x)
  out
}


#' Apply the bottom border of the input table to the output
#'
#' @param out Output sliced table
#' @param x Base table being sliced
#' @param columns Subset of cols
#' @noRd
reapply_bottom_border <- function(out, x, columns) {
  for (n in names(x)[endsWith(names(x), ".bottom")]) {
    out[[n]]$data[out[[n]]$nrow, ] <- x[[n]]$data[x[[n]]$nrow, columns]
  }
  out
}

# TODO: This needs to be able to accept column names instead of just indices
# Saving this for a broader switch later
# slice_clintable2 <- function(x, rows, columns) {
#   x <- flextable::delete_columns(x, j = x$col_keys[!(x$col_keys %in% columns)])
#   out_rows <- 1:nrow(x$body$data)
#   out_rows <- out_rows[!(out_rows %in% rows)]
#   x <- flextable::delete_rows(x, i = out_rows)
#   x
# }
