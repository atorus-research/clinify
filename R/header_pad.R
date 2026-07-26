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
#' `above` and `below` apply to every row of the header, which is the usual
#' convention and matches `flextable::padding(part = "header")`. To space a
#' single header row differently, reach for `flextable::padding()` with an `i`
#' directly.
#'
#' `rule_to_body` is applied to the first row of every page, so a table split
#' over pages keeps the same gap under the rule throughout. If a group label is
#' added above the header it keeps its own spacing, since it is put there as
#' the table renders.
#'
#' Spacing is given in points, which is what flextable measures cell padding
#' in. Whatever is set here replaces the header padding clinify starts with.
#'
#' @param x A clintable object
#' @param above Space above each header row, in points
#' @param below Space below each header row, in points. The bottom row's is
#'   what sets how far the rule sits from the column labels
#' @param rule_to_body Space between that rule and the first body row, in
#'   points
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
  rule_to_body = NULL
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

  x$clinify_config$header_pad <- pad
  x
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
      length(value) != 1 ||
      is.na(value) ||
      value < 0
  ) {
    stop(
      "`",
      edge,
      "` must be a single number of points that is not negative, not ",
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

  if (flextable::nrow_part(x, part = "header") < 1) {
    return(x)
  }

  if (!is.null(pad$above)) {
    x <- flextable::padding(x, part = "header", padding.top = pad$above)
  }

  if (!is.null(pad$below)) {
    x <- flextable::padding(x, part = "header", padding.bottom = pad$below)
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
