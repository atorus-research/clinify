#' Set the spacing around a table's column headers
#'
#' Three pieces of vertical space shape the header block, and they are named
#' here for where they sit rather than for the padding that produces them,
#' because the mapping between the two is not obvious:
#'
#' - `above` is the space over the column labels. A blank row above the header
#'   is a common house convention, and padding is how it is produced.
#' - `label_to_rule` is the space between the column labels and the rule drawn
#'   under them. A cell's bottom border sits at the bottom edge of the cell,
#'   *below* its bottom padding, so padding under the header pushes the rule
#'   away from the labels and toward the body. It does not open space beneath
#'   the rule.
#' - `rule_to_body` is the space between that rule and the first row of the
#'   table body, which is the one that has to come from the body side.
#'
#' clinify starts every table with 9 points above the top header row and 9
#' below the bottom one. Whatever is given here replaces those.
#'
#' `rule_to_body` is applied to the first row of every page, so a table split
#' over pages keeps the same gap under the rule throughout. If a group label
#' is added above the header, `above` stays with the column labels, which
#' leaves it as the space between the label and the labels beneath it.
#'
#' Spacing is given in points, which is what flextable measures cell padding
#' in.
#'
#' @param x A clintable object
#' @param above Space above the column labels, in points
#' @param label_to_rule Space between the column labels and the rule under
#'   them, in points
#' @param rule_to_body Space between that rule and the first body row, in
#'   points
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' # A blank row's worth of space above the labels, the rule tight underneath
#' # them, and a little air before the body starts
#' clintable(mtcars) |>
#'   clin_header_pad(above = 18, label_to_rule = 4, rule_to_body = 6)
clin_header_pad <- function(
  x,
  above = NULL,
  label_to_rule = NULL,
  rule_to_body = NULL
) {
  stopifnot(inherits(x, "clintable"))

  pad <- list(
    above = above,
    label_to_rule = label_to_rule,
    rule_to_body = rule_to_body
  )

  if (all(vapply(pad, is.null, TRUE))) {
    stop(
      "At least one of above, label_to_rule, or rule_to_body needs a value"
    )
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
#' The header travels onto every page, so this only needs doing once, before
#' the table is sliced.
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

  rows <- flextable::nrow_part(x, part = "header")

  if (rows < 1) {
    return(x)
  }

  if (!is.null(pad$above)) {
    x <- flextable::padding(
      x,
      i = 1,
      part = "header",
      padding.top = pad$above
    )
  }

  if (!is.null(pad$label_to_rule)) {
    x <- flextable::padding(
      x,
      i = rows,
      part = "header",
      padding.bottom = pad$label_to_rule
    )
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
