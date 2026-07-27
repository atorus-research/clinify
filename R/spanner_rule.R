#' Draw a rule beneath the spanners in a clintable's column headers
#'
#' Regulatory arm spanner tables carry a thin horizontal rule directly under
#' each spanner label, running across only the columns that spanner covers, so
#' that the label reads as a heading over its own block of columns. The columns
#' are worked out from the header that is on the table, so the rule follows the
#' spanners as the layout changes instead of having to be given as column
#' numbers that then have to be kept in step with it.
#'
#' A spanner is a run of header cells that has been merged together, which is
#' what `clin_column_headers()` makes of adjacent cells holding the same text.
#' Two kinds of run are deliberately left alone:
#'
#' - A run of blank cells. That is the empty space over a stub column, or over
#'   a trailing p-value column, rather than a spanner - clinify fills the header
#'   levels a column does not use, so those cells merge into a run of their own.
#' - Anything in the bottom row of the header. That row holds the individual
#'   column labels, so a merged run in it is a label sitting over two columns
#'   rather than a spanner, and the rule under the bottom row is the one the
#'   styling function draws across the whole table.
#'
#' Called a second time, this refines what the first call set rather than
#' replacing it: arguments this call does not name keep their earlier value.
#'
#' The rule is drawn as the table renders, after the default styling function
#' has run. That is what makes it survive a house style: the stock
#' `clinify_table_default()` opens with `flextable::border_remove()`, which
#' would wipe a border applied any earlier, and a house style is free to draw
#' its header rules in a pen of its own.
#'
#' @param x A clintable object
#' @param border The pen to draw the rule with. `TRUE`, the default, draws the
#'   1pt solid black rule these tables conventionally use. An
#'   `officer::fp_border()` draws in whatever width, style and colour it
#'   carries, which is how to get a dashed or a hairline rule. `FALSE` draws no
#'   rule, which is how to stop a house style from underlining the spanners.
#' @param rows Header rows to rule, numbered from the top down. `NULL`, the
#'   default, rules every row above the bottom one, so a header of any depth
#'   has all of its spanners underlined.
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' df <- data.frame(
#'   stub = c("Male", "Female"),
#'   a_lo = c("5 (10%)", "7 (14%)"),
#'   a_hi = c("2 (4%)", "3 (6%)"),
#'   b_lo = c("6 (12%)", "8 (16%)"),
#'   b_hi = c("1 (2%)", "4 (8%)")
#' )
#'
#' ct <- clintable(df, use_labels = FALSE) |>
#'   clin_column_headers(
#'     stub = "",
#'     a_lo = c("Drug A (N=50)", "Low"),
#'     a_hi = c("Drug A (N=50)", "High"),
#'     b_lo = c("Drug B (N=50)", "Low"),
#'     b_hi = c("Drug B (N=50)", "High")
#'   )
#'
#' # A rule under each arm spanner, over that arm's two columns only, with the
#' # stub left un-ruled
#' clin_spanner_rule(ct)
#'
#' # The same rule in a dashed pen
#' clin_spanner_rule(ct, border = officer::fp_border(style = "dashed"))
clin_spanner_rule <- function(x, border = TRUE, rows = NULL) {
  stopifnot(inherits(x, "clintable"))

  # Only what this call actually named, so a second call refines the first.
  # `border` has a default, so being NULL cannot say if it was asked for
  supplied <- list()

  if (!missing(border)) {
    supplied$border <- check_spanner_border_(border)
  }

  if (!missing(rows)) {
    supplied$rows <- check_spanner_rows_(rows)
  }

  x$clinify_config$spanner_rule <- merge_config_(
    x$clinify_config$spanner_rule,
    supplied
  )

  # A first call that named nothing still means "rule the spanners"
  if (is.null(x$clinify_config$spanner_rule$border)) {
    x$clinify_config$spanner_rule$border <- check_spanner_border_(border)
  }

  x
}

#' Check the pen the rule is to be drawn with
#'
#' `FALSE` becomes a border that is there but draws nothing, which is how
#' flextable takes a border away - the same way the stock
#' `clinify_table_default()` clears the rule from under a blank header cell.
#'
#' @param border The pen as given
#'
#' @return An fp_border object
#'
#' @noRd
check_spanner_border_ <- function(border) {
  if (inherits(border, "fp_border")) {
    return(border)
  }

  if (is.logical(border) && length(border) == 1 && !is.na(border)) {
    if (border) {
      return(officer::fp_border())
    }
    return(officer::fp_border(style = "none", width = 0))
  }

  stop(
    "`border` must be TRUE, FALSE, or an officer::fp_border(), not ",
    paste(deparse(border), collapse = "")
  )
}

#' Check the header rows the rule is to be drawn under
#'
#' Only the shape of the request is checked here. Which rows a header actually
#' has is not settled until it renders, because the header can be built after
#' this is set.
#'
#' @param rows The rows as given
#'
#' @return An integer vector of header rows, or NULL for all of them
#'
#' @noRd
check_spanner_rows_ <- function(rows) {
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

#' Resolve the requested rows against the header the table has
#'
#' The bottom row of the header is not one of these, so a header one row deep
#' has no rows to rule at all and asking for none is not an error - a table
#' with no spanners simply gets no rule.
#'
#' @param rows The requested rows, or NULL for all of them
#' @param depth The number of header rows
#'
#' @return An integer vector of header rows to rule
#'
#' @noRd
spanner_rule_rows_ <- function(rows, depth) {
  spanner_rows <- seq_len(depth - 1)

  if (is.null(rows)) {
    return(spanner_rows)
  }

  if (!all(rows %in% spanner_rows)) {
    if (depth < 2) {
      stop(
        "`rows` cannot be used on a clintable whose column header is a single ",
        "row deep - it has no rows above its bottom row to rule"
      )
    }

    stop(sprintf(
      paste(
        "`rows` must be header row numbers between 1 and %s - the rows above",
        "the bottom row of the header, which holds the column labels rather",
        "than spanners"
      ),
      depth - 1
    ))
  }

  rows
}

#' Find the merged runs of header cells that are spanners
#'
#' A header row's spans hold the width of a merged run on its first cell and a
#' zero on every cell the run covers, so a cell of its own reads as a run one
#' column wide. A run has to be at least two columns wide to be a spanner, and
#' it has to say something - the blank runs are the space clinify fills in over
#' the header levels a column does not use.
#'
#' @param spans The `spans$rows` matrix of the header
#' @param text The header text, as a matrix
#' @param rows The header rows to look in
#'
#' @return A list of runs, each a list of the row it is in and the columns it
#'   covers
#'
#' @noRd
spanner_runs_ <- function(spans, text, rows) {
  runs <- list()

  for (i in rows) {
    for (j in which(spans[i, ] > 1)) {
      if (!nzchar(trimws(text[i, j]))) {
        next
      }

      runs[[length(runs) + 1]] <- list(
        row = i,
        cols = seq(j, j + spans[i, j] - 1)
      )
    }
  }

  runs
}

#' Draw the configured rule under the header's spanners
#'
#' Run after the default styling function, so the rule holds whatever that
#' function did to the table's borders. The header travels onto every page, so
#' this only needs doing once, before the table is sliced.
#'
#' @param x A clintable object
#'
#' @return A clintable object
#'
#' @noRd
apply_spanner_rule_ <- function(x) {
  cfg <- x$clinify_config$spanner_rule

  if (is.null(cfg)) {
    return(x)
  }

  depth <- flextable::nrow_part(x, part = "header")

  if (depth < 1) {
    return(x)
  }

  rows <- spanner_rule_rows_(cfg$rows, depth)
  runs <- spanner_runs_(
    x$header$spans$rows,
    as.matrix(x$header$dataset),
    rows
  )

  for (run in runs) {
    x <- flextable::hline(
      x,
      i = run$row,
      j = run$cols,
      border = cfg$border,
      part = "header"
    )
  }

  x
}
