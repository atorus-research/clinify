#' Set the column headers of the output clintable
#'
#' This function allows you to apply column headers named arguments
#' and character vectors. Separate elements of the character vector
#' are converted to separate levels of the output table header.
#' The in which the headers are applied goes from top to bottom,
#' so if you provide 3 elements for a column header, the first
#' element is applied to the top and the second to the bottom.
#' If one variable has three levels and other variable only have
#' one or two, the columns with less levels to the header will bind
#' to the bottom. So a column with two levels will apply to the
#' second and third row, and a column with one level with apply
#' the bottom row. Spanners are determined using cells of the same
#' text value, where horizontally adjacent cells holding the same
#' text are merged. Use the `merge` argument when a header row
#' legitimately repeats a label across adjacent columns and those
#' cells should be left alone - merged, they render as one label
#' centred over the whole run, so the repeats are not there to read
#' any more. That is most often wanted for the bottom row, which
#' holds each column's own label: six columns each labelled
#' `"Baseline"` come out as a single `Baseline` spanning all six
#' unless `merge = "spanners"` keeps that row out of it. `merge`
#' works a row at a time, so if a single row needs some of its
#' repeated cells merged but not others, leave that row out of
#' `merge` and span the intended cells with
#' `flextable::merge_at()`.
#'
#' The same result can be achieved using column labels on the
#' input dataframe to the clintable. If labels are present,
#' header levels will be separated using the delimitter "||" within
#' the label string. Headers built that way can have their merging
#' adjusted by calling `clin_column_headers()` with no header text and
#' only the `merge` argument, which leaves the header text as it is.
#' Called that way, any merging already on the header is cleared first -
#' including merges applied by hand with `flextable::merge_at()` or
#' `flextable::merge_v()` - so the rows named in `merge` end up being the
#' only merged rows.
#'
#' @param x A clintable object
#' @param ... Named arguments providing the column header text.
#'   Separate levels of the header are determined using separate
#'   elements of a character vector.
#' @param merge Controls the automatic merging of identical, adjacent
#'   header cells, which is what forms spanners. `TRUE` (the default) or
#'   `"all"` merges every header row, `FALSE` or `"none"` merges none of
#'   them, and `"spanners"` merges every row except the bottom one - the
#'   row holding the individual column labels. Merging can also be
#'   limited to specific header rows, numbered from the top down, using
#'   ordinary R subscripts: `merge = 1:2` merges the top two rows only,
#'   `merge = -3` merges every row except the third, and a logical vector
#'   as long as the header is deep toggles each row individually. Only
#'   the header is ever merged - the table body is left alone.
#'
#'   One thing to know: a custom `clinify_table_default()` that calls
#'   `flextable::merge_h()` on the header will merge it again when the
#'   table renders, overriding whatever is set here.
#'
#' @return A clintable object
#'
#' @export
#' @examples
#'
#' clintable(iris) |>
#'   clin_column_headers(
#'     Sepal.Length = c("Flowers", "Sepal", "Length"),
#'     Sepal.Width = c("Flowers", "Sepal", "Width"),
#'     Petal.Length = c("Petal", "Length"),
#'     Petal.Width = c("Petal", "Width")
#'   )
#'
#' # Keep the repeated bottom row cells separate, but still span
#' # "Flowers" and "Petal" across the columns above them
#' clintable(iris) |>
#'   clin_column_headers(
#'     Sepal.Length = c("Flowers", "Sepal", "Value"),
#'     Sepal.Width = c("Flowers", "Sepal", "Value"),
#'     Petal.Length = c("Petal", "Value"),
#'     Petal.Width = c("Petal", "Value"),
#'     merge = "spanners"
#'   )
#'
#' # Headers coming from column labels can have their merging adjusted
#' # without restating the header text
#' iris2 <- iris
#' attr(iris2$Sepal.Length, "label") <- "Flowers||Value"
#' attr(iris2$Sepal.Width, "label") <- "Flowers||Value"
#'
#' clintable(iris2) |>
#'   clin_column_headers(merge = 1)
#'
clin_column_headers <- function(x, ..., merge = TRUE) {
  stopifnot(inherits(x, "clintable"))

  # A column of the data named `merge` would be claimed by that parameter, so
  # there is no telling a header for it apart from a merging instruction
  if (!missing(merge) && "merge" %in% colnames(x$body$dataset)) {
    stop(
      "The `merge` argument cannot be used on a clintable that has a column ",
      "named `merge`. Set that column's header using column labels instead ",
      "(see the `use_labels` argument of `clintable()`)."
    )
  }

  # Pull out the header text
  args <- list(...)
  hint <- merge_hint_(names(args), colnames(x$body$dataset))

  if (!all(vapply(args, is.character, TRUE))) {
    stop("All header arguments must be characters", hint)
  }

  if (!all(names(args) %in% colnames(x$body$dataset))) {
    stop(
      "All argument names must be columns present within the clintable columns",
      hint
    )
  }

  # Without any header text there's nothing to build, so take it as a request
  # to re-apply the merging rules to the headers already in place. This is how
  # headers built from column labels are adjusted.
  if (length(args) == 0) {
    return(remerge_header_(x, merge))
  }

  apply_column_headers_(x, args, merge)
}

#' Fill the unused levels of one column's header
#'
#' Blanks are padded with a single space when the column has something in
#' it, and left empty when the whole column is blank. That is what keeps an
#' entirely blank column from merging into the padding of the column beside
#' it, which in turn keeps the bottom border off it.
#'
#' @param x The header levels of one column, unused levels being `NA`
#'
#' @return The header levels with the blanks filled in
#'
#' @noRd
fill_header_blanks_ <- function(x) {
  if (all(is.na(x)) || any(x != "", na.rm = TRUE)) {
    x[is.na(x)] <- " "
  } else {
    x[is.na(x)] <- ""
  }
  x
}

#' Nudge towards `merge` when a header argument looks like a misspelling of it
#'
#' `merge` follows `...`, so R will not partially match it. A near miss on
#' the name silently becomes header text for a column that doesn't exist,
#' which is worth pointing out.
#'
#' @param nms Names of the header arguments that were supplied
#' @param cols Columns of the clintable
#'
#' @return A sentence to append to an error message, or an empty string
#'
#' @noRd
merge_hint_ <- function(nms, cols) {
  # A name that really is a column of the data is not a misspelling
  nms <- setdiff(nms[nzchar(nms)], cols)
  nms <- tolower(nms)

  near_miss <- (startsWith("merge", nms) & nchar(nms) >= 3) |
    startsWith(nms, "merge")

  if (any(near_miss)) {
    paste(
      ". Did you mean `merge`? Arguments that follow `...` have to be",
      "named in full"
    )
  } else {
    ""
  }
}

#' Apply a set of column headers to a clintable
#'
#' Shared workhorse of `clin_column_headers()` and
#' `headers_from_labels_()`. Header text arrives as a list rather than
#' through `...` so that a variable named the same as one of the
#' function parameters can still be given a header.
#'
#' @param x A clintable object
#' @param args A named list of character vectors of header text
#' @param merge Header rows to merge, as documented in `clin_column_headers()`
#'
#' @return A clintable object
#'
#' @noRd
apply_column_headers_ <- function(x, args, merge = TRUE) {
  refdat <- x$body$dataset

  if (!all(names(args) %in% colnames(refdat))) {
    stop("All argument names must be columns present within the clintable columns")
  }

  # Find how many header levels are necessary
  depth <- max(vapply(args, length, 1))

  if (depth < 1) {
    stop("Column headers must have at least one level")
  }

  # Create a matrix for the headers
  mheaders <- matrix(nrow = depth, ncol = ncol(refdat))
  colnames(mheaders) <- names(refdat)

  # Loop the arguments provided
  for (n in names(args)) {
    # Start at the bottom level
    i <- depth
    # Insert elements moving bottom to top
    for (h in rev(args[[n]])) {
      mheaders[i, n] <- h
      i <- i - 1
    }
  }

  # Fill the characters. Rebuilding the matrix by hand rather than leaning on
  # what apply() gives back keeps the shape the same whether the header is one
  # level deep or the table is one column wide
  mheaders <- matrix(
    unlist(
      lapply(seq_len(ncol(mheaders)), \(j) fill_header_blanks_(mheaders[, j])),
      use.names = FALSE
    ),
    nrow = depth,
    ncol = ncol(refdat)
  )

  # The typology wants a row per column of the table
  typology <- as.data.frame(t(mheaders), row.names = FALSE)
  typology["col_keys"] <- colnames(refdat)

  # Apply to the clintable
  x <- flextable::set_header_df(x, typology)

  # set_header_df() rebuilds the header part, so the spacing clinify starts a
  # header with has just been dropped and needs putting back
  x <- default_header_pad_(x)

  # Merging is resolved against the header rows that actually landed on the
  # table, which is not always the number of levels asked for
  remerge_header_(x, merge, clear = FALSE)
}

#' Merge the rows of a header that is already in place
#'
#' Rows are resolved against the header rows the table actually has,
#' rather than the number of levels the caller asked for - the two are
#' not always the same. Used directly when `clin_column_headers()` is
#' given no header text, which leaves the header text alone and only
#' changes which rows have their identical, adjacent cells merged.
#'
#' @param x A clintable object
#' @param merge Header rows to merge, as documented in `clin_column_headers()`
#' @param clear Clear any merging already in place first, so the requested
#'   rows end up being the only merged rows
#'
#' @return A clintable object
#'
#' @noRd
remerge_header_ <- function(x, merge, clear = TRUE) {
  depth <- nrow(x$header$dataset)

  if (depth < 1) {
    stop(
      "Column headers cannot be merged because the clintable has no header rows"
    )
  }

  merge_rows <- unique(resolve_header_merge_(merge, depth))

  if (clear) {
    x <- flextable::merge_none(x, part = "header")

    # merge_none() hands back doubles where the rest of the header spans are
    # integers, which would leave two equivalent routes comparing unequal
    storage.mode(x$header$spans$rows) <- "integer"
    storage.mode(x$header$spans$columns) <- "integer"
  }

  if (length(merge_rows) > 0) {
    x <- flextable::merge_h(x, i = merge_rows, part = "header")
  }

  x
}

#' Keywords accepted by the `merge` argument
#' @noRd
merge_keywords_ <- c("all", "none", "spanners")

#' Message for a `merge` value that is not something we can work with
#' @noRd
merge_type_msg_ <- paste0(
  "`merge` must be TRUE, FALSE, ",
  paste(sprintf('"%s"', merge_keywords_), collapse = ", "),
  ", or a vector of header row numbers"
)

#' Resolve the `merge` argument into header row numbers
#'
#' Standard R subscript semantics are used against the header rows, so
#' `TRUE`/`FALSE` select all or none, positive numbers select rows from
#' the top down, negative numbers drop rows, and a logical vector as
#' long as the header is deep toggles each row.
#'
#' @param merge The user provided `merge` value
#' @param depth The number of header rows
#'
#' @return An integer vector of header rows to merge
#'
#' @noRd
resolve_header_merge_ <- function(merge, depth) {
  if (is.null(merge)) {
    stop("`merge` cannot be NULL. ", merge_type_msg_)
  }

  # Keywords say what the merging is for rather than where it lands, so they
  # hold up when a header gains or loses a level
  if (is.character(merge)) {
    if (length(merge) != 1 || !merge %in% merge_keywords_) {
      stop(merge_type_msg_)
    }

    return(switch(
      merge,
      all = seq_len(depth),
      none = integer(0),
      spanners = seq_len(depth - 1)
    ))
  }

  if (!is.logical(merge) && !is.numeric(merge)) {
    stop(merge_type_msg_)
  }

  if (length(merge) == 0 || anyNA(merge)) {
    stop("`merge` cannot be empty or contain missing values")
  }

  if (is.logical(merge)) {
    if (!length(merge) %in% c(1, depth)) {
      stop(sprintf(
        "A logical `merge` must be length 1 or length %s (the number of header rows), not %s",
        depth,
        length(merge)
      ))
    }
  } else {
    if (any(merge != trunc(merge))) {
      stop("`merge` header row numbers must be whole numbers")
    }
    if (any(merge > 0) && any(merge < 0)) {
      stop("`merge` cannot mix positive and negative header row numbers")
    }
    if (any(merge == 0)) {
      stop("`merge` header row numbers cannot be 0 - header rows count from 1")
    }
    if (any(abs(merge) > depth)) {
      stop(sprintf(
        "`merge` header row numbers must be between 1 and %s (the number of header rows)",
        depth
      ))
    }
  }

  seq_len(depth)[merge]
}

#' Convert column labels into column headers
#'
#' This function will look at the column labels, and if present
#' separate the header levels using the delimitter "||" within
#' the label string. Header setup is done using the same machinery as
#' the exported function `clin_column_headers()`. Spanners are
#' determined using cells of the same text value, where horizontally
#' adjacent cells holding the same text are merged.
#'
#' @param x A clintable object
#' @param merge Header rows to merge, as documented in `clin_column_headers()`
#'
#' @return A clintable object
#'
#' @noRd
headers_from_labels_ <- function(x, merge = TRUE) {
  refdat <- x$body$dataset
  if (has_labels_(refdat)) {
    args <- lapply(refdat, \(x) {
      # exact = TRUE, or a `labels` attribute of value labels answers a request
      # for `label` and gets read as header text
      label <- attr(x, "label", exact = TRUE)

      if (!is.null(label)) {
        unlist(strsplit(label, "||", fixed = TRUE))
      } else {
        ""
      }
    })

    # Build header df from the labels
    apply_column_headers_(x, args, merge)
  } else {
    # Just return the object if no labels
    x
  }
}

#' Do any of the dataframe variables have labels?
#' @noRd
has_labels_ <- function(x) {
  # exact = TRUE, because attr() otherwise partial matches - haven attaches a
  # `labels` attribute of value labels to coded variables, and that would
  # answer a request for `label` and be mistaken for a variable label
  any(vapply(x, \(y) !is.null(attr(y, "label", exact = TRUE)), FALSE))
}
