#' Set the spacing around a table's column headers
#'
#' Three pieces of vertical space shape the header block, and they are named
#' here for where they sit rather than for the padding that produces them,
#' because the mapping between the two is not obvious:
#'
#' - `above` is the space over each header row. On a single row header that is
#'   the buffer above the column labels; on a spanned header it also opens the
#'   space between the levels, which is what a blank row above the header
#'   normally looks like.
#' - `below` is the space under each header row. The one that matters most is
#'   the bottom row's, because a cell's bottom border sits at the bottom edge
#'   of the cell, *below* its padding - so this is what decides how far the
#'   rule is drawn from the column labels. It does not open space beneath the
#'   rule.
#' - `rule_to_body` is the space between that rule and the first row of the
#'   table body, which is the one that has to come from the body side.
#'
#' `above` and `below` apply to every row of the header by default, which is
#' the usual convention and matches `flextable::padding(part = "header")`. A
#' header row that needs a different gap can be given one either by passing a
#' value per row - `above = c(18, 34)` - or by aiming the call at particular
#' rows with `rows`, which leaves the others alone. That matters because the
#' spacing is applied as the table renders, after anything the caller did, so a
#' call covering every row would otherwise overwrite a per-row
#' `flextable::padding()` set beforehand.
#'
#' `rule_to_body` is applied to the first row of every page, so a table split
#' over pages keeps the same gap under the rule throughout. If a group label is
#' added above the header it keeps its own spacing, since it is put there as
#' the table renders.
#'
#' Called a second time, this refines what the first call set rather than
#' replacing it: arguments this call does not name keep their earlier value.
#'
#' Spacing is given in points, which is what flextable measures cell padding
#' in. Whatever is set here replaces the header padding clinify starts with.
#'
#' @param x A clintable object
#' @param above Space above each header row, in points
#' @param below Space below each header row, in points. The bottom row's is
#'   what sets how far the rule sits from the column labels
#' @param rule_to_body Space between that rule and the first body row, in
#'   points. A single value - there is only one first body row per page
#' @param rows Which header rows to space, as row numbers counting from the
#'   top. The default spaces every row; rows left out keep whatever spacing
#'   they already have
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' # A blank row's worth of space around each header row, the rule close under
#' # the labels, and a little air before the body starts
#' clintable(mtcars) |>
#'   clin_header_pad(above = 18, below = 4, rule_to_body = 6)
clin_header_pad <- function(
  x,
  above = NULL,
  below = NULL,
  rule_to_body = NULL,
  rows = NULL
) {
  stopifnot(inherits(x, "clintable"))

  pad <- list(
    above = above,
    below = below,
    rule_to_body = rule_to_body
  )

  if (all(vapply(pad, is.null, TRUE))) {
    stop("At least one of above, below, or rule_to_body needs a value")
  }

  for (edge in names(pad)) {
    pad[[edge]] <- check_header_pad_(pad[[edge]], edge)
  }

  # A single value for `rule_to_body`, since there is only ever one first body
  # row to space away from the rule
  if (length(pad$rule_to_body) > 1) {
    stop("`rule_to_body` must be a single number of points")
  }

  # Only what this call actually named, so a second call refines the first
  named <- c(
    if (!missing(above)) "above",
    if (!missing(below)) "below",
    if (!missing(rule_to_body)) "rule_to_body"
  )

  supplied <- pad[named]

  if (!missing(rows)) {
    supplied$rows <- check_header_pad_rows_(rows)
  }

  x$clinify_config$header_pad <- merge_config_(
    x$clinify_config$header_pad,
    supplied
  )
  x
}

#' Check the header rows a spacing request is aimed at
#'
#' @param rows The `rows` argument as given
#'
#' @return Header row numbers, or NULL for every row
#'
#' @noRd
check_header_pad_rows_ <- function(rows) {
  if (is.null(rows)) {
    return(NULL)
  }

  if (
    !is.numeric(rows) ||
      length(rows) == 0 ||
      anyNA(rows) ||
      any(rows != trunc(rows)) ||
      any(rows < 1)
  ) {
    stop(
      "`rows` must be whole header row numbers of 1 or more, not ",
      paste(deparse(rows), collapse = "")
    )
  }

  as.integer(unique(rows))
}

#' Check one header spacing value
#'
#' @param value The spacing as given
#' @param edge Which of the three it is, for the message
#'
#' @return The spacing, or NULL
#'
#' @noRd
check_header_pad_ <- function(value, edge) {
  if (is.null(value)) {
    return(NULL)
  }

  if (
    !is.numeric(value) ||
      length(value) == 0 ||
      anyNA(value) ||
      any(value < 0)
  ) {
    stop(
      "`",
      edge,
      "` must be numbers of points that are not negative, not ",
      paste(deparse(value), collapse = "")
    )
  }

  value
}

#' Apply the header spacing that belongs to the header
#'
#' Every header row is spaced the same, so a spanned header keeps the space
#' between its levels rather than only at the outside of the block. The header
#' travels onto every page, so this only needs doing once, before the table is
#' sliced.
#'
#' @param x A clintable object
#'
#' @return A clintable object
#'
#' @noRd
apply_header_pad_ <- function(x) {
  pad <- x$clinify_config$header_pad

  if (is.null(pad)) {
    return(x)
  }

  depth <- flextable::nrow_part(x, part = "header")

  if (depth < 1) {
    return(x)
  }

  rows <- pad$rows

  if (is.null(rows)) {
    rows <- seq_len(depth)
  } else if (!all(rows <= depth)) {
    stop(sprintf(
      paste(
        "`rows` must be header row numbers between 1 and %s, the number of",
        "rows this header has"
      ),
      depth
    ))
  }

  x <- pad_header_rows_(x, rows, pad$above, "padding.top")
  pad_header_rows_(x, rows, pad$below, "padding.bottom")
}

#' Space the header rows a request is aimed at, and only those
#'
#' Leaving the other rows alone is what lets a house wide buffer sit alongside
#' the handful of tables that need a different gap on one row.
#'
#' @param x A clintable object
#' @param rows The header rows in scope
#' @param value One spacing for all of them, or one each
#' @param side Either "padding.top" or "padding.bottom"
#'
#' @return A clintable object
#'
#' @noRd
pad_header_rows_ <- function(x, rows, value, side) {
  if (is.null(value)) {
    return(x)
  }

  if (!length(value) %in% c(1, length(rows))) {
    stop(sprintf(
      paste(
        "Header spacing must be one value, or one for each of the %s rows it",
        "is aimed at, not %s"
      ),
      length(rows),
      length(value)
    ))
  }

  value <- rep_len(value, length(rows))

  for (i in seq_along(rows)) {
    args <- list(x, i = rows[i], part = "header")
    args[[side]] <- value[i]
    x <- do.call(flextable::padding, args)
  }

  x
}

#' Apply the gap under the header rule to a rendered page
#'
#' This one has to come from the body side, and from the first row of each
#' page rather than the first row of the table.
#'
#' @param ft A clinpage object
#' @param pad The header spacing configuration
#'
#' @return A clinpage object
#'
#' @noRd
apply_rule_to_body_ <- function(ft, pad) {
  if (is.null(pad) || is.null(pad$rule_to_body)) {
    return(ft)
  }

  if (flextable::nrow_part(ft, part = "body") < 1) {
    return(ft)
  }

  flextable::padding(
    ft,
    i = 1,
    part = "body",
    padding.top = pad$rule_to_body
  )
}
