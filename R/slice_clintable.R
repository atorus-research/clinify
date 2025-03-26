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
#'
#' @return A clinpage object
#'
#' @examples
#' x <- flextable::flextable(mtcars)
#' y <- slice_clintable(x, 20:32, c(1:3, 5:8))
#' z <- flextable::flextable(mtcars[20:32, c(1:3, 5:8)])
#' @noRd
slice_clintable <- function(x, rows, columns, skip_spans=FALSE) {
  out <- new_clinpage()

  if (!is.null(names(columns))) {
    columns <- eval_select(names(columns), x$body$dataset)
  }
  
  out$header <- slice_complex_tabpart(x$header, 1:nrow(x$header$dataset), columns)
  out$footer <- slice_complex_tabpart(x$footer, 1:nrow(x$footer$dataset), columns)
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

  # If the column headers have spanners, then it's possible 
  # the horizontal span is too wide for the vector
  if (!skip_spans) {
    for (i in seq_along(dim(out$header$spans$rows)[1])) {
      out$header$spans$rows[i,] <- adjust_span_row(out$header$spans$rows[i, ], out$header$dataset[i, ])
    }
  }

  out
}

#' Adjust Row Spanning Over Page Breaks
#'
#' This function modifies a vector `v` representing row spans in a table. It ensures that 
#' spans are correctly adjusted over page breaks by identifying cases where data values exist (`d`), 
#' but spans are incorrectly set to 0. The function updates such cases to maintain consistency.
#'
#' @param v A numeric vector representing row spans.
#' @param d A character vector representing the data values in the table.
#' @return A modified numeric vector with adjusted row spans.
#' @examples
#' v <- c(0, 0, 2, 0, 0)
#' d <- c("A", "A", "B", "B", "B")
#' adjust_span_row(v, d)  # Corrects the spans accordingly
#' @noRd
adjust_span_row <- function(v, d) {
  # Find and update spans over page breaks by finding
  # data values populated but spans are set as 0
  not_empty <- !(d %in% c(" ", ""))
  if (any(not_empty)) {
    ne_ind <- which(not_empty)
    
    indices <- which(diff(ne_ind) == 1)  # Find indices where the difference is 1
    unique_starts <- indices[which(diff(indices) != 1)]
    
    # Include the first element if it starts a sequence
    if (length(indices) > 0 && indices[1] == 1) {
      unique_starts <- c(ne_ind[1], unique_starts)
    }

    for (i in unique_starts) {
      # Set it to 2 and this will correct in the next step
      if (v[i] == 0) v[i] <- 2
    }
  }
  
  for (i in seq_along(v)) {
    if (
      (
        # Contains multi span and not the last element and not, or
        (v[i] > 0 && i < length(v) && v[i] != 1) || 
        # The last element is 1 but this one is 0, so this is spanned
        (i == 1 || v[i-1] == 1) && v[i] == 0
      ) && 
        # Followed by 0
      v[i + 1] == 0
    ) {
      # Count consecutive zeros after v[i]
      count_zeros <- sum(cumsum(v[-(1:i)]) == 0)  
      v[i] <- count_zeros + 1
    } else {
      # For the last element don't span extra
      if (i == length(v) && v[i] > 1) {
        v[i] <- 1
      }
    }
  }
  return(v)
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

  dataset <- x$dataset[rows, columns]

  # Content element
  content <- slice_chunkset_struct(x$content, rows, columns)

  col_keys <- x$col_keys[columns]
  colwidths <- x$colwidths[columns]
  rowheights <- x$rowheights[rows]
  hrule <- x$hrule[rows]

  # Spans
  spans <- x$spans
  spans$rows <- spans$rows[rows, columns, drop = FALSE]
  spans$columns <- spans$columns[rows, columns, drop = FALSE]

  # Styles
  styles <- list(cells = NULL, pars = NULL, text = NULL)

  styles$cells <- lapply(x$styles$cells,
    slice_fpstruct,
    rows = rows,
    columns = columns
  )
  styles$pars <- lapply(x$styles$pars,
    slice_fpstruct,
    rows = rows,
    columns = columns
  )
  styles$text <- lapply(x$styles$text,
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
