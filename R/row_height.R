#' Set the row height of a clintable
#'
#' Regulatory outputs are usually specified to an exact row pitch, and the
#' rendered height of a row is what decides how much fits on a page. flextable
#' leaves rows at a nominal quarter inch with a rule of "auto", which lets the
#' renderer size them however it likes. This records the pitch you want and
#' applies it when the table renders.
#'
#' The three surfaces are set separately because they are separate tables: the
#' table body, and the title and footnote blocks that go into the Word header
#' and footer. Group label and caption rows, which clinify inserts while it
#' renders, take the body pitch.
#'
#' The height is applied to whole parts, so it is a pitch for every row of the
#' surface rather than a per-row height. Anything already set with
#' `flextable::height()` or `flextable::height_all()` is replaced. Because the
#' pitch is applied after the default styling functions run, it also holds when
#' an organisation's `clinify_table_default()` sets a house pitch of its own -
#' the table's own setting wins.
#'
#' @param x A clintable object
#' @param body Row pitch for the table body
#' @param title Row pitch for the title lines
#' @param footnote Row pitch for the footnote lines, and for a footnote page
#' @param rule How the renderer should treat the pitch. `"atleast"` (the
#'   default) makes it a floor, so a cell whose text wraps grows past it instead
#'   of being clipped. `"exact"` pins the row to the pitch and clips anything
#'   that does not fit, which is the only way to get a pitch at or below the
#'   font's line height. `"auto"` lets the renderer decide, ignoring the pitch.
#' @param unit Unit the pitches are given in. Row pitch is normally specified in
#'   points, so that is the default.
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' clintable(mtcars) |>
#'   clin_row_height(body = 15.35, title = 11.4, footnote = 11.4)
#'
#' # Or in inches
#' clintable(mtcars) |>
#'   clin_row_height(body = 0.213, unit = "in")
clin_row_height <- function(
  x,
  body = NULL,
  title = NULL,
  footnote = NULL,
  rule = c("atleast", "exact", "auto"),
  unit = c("pt", "in", "cm", "mm")
) {
  stopifnot(inherits(x, "clintable") || inherits(x, "clindoc"))
  rule <- match.arg(rule)
  unit <- match.arg(unit)

  heights <- list(body = body, title = title, footnote = footnote)

  if (all(vapply(heights, is.null, TRUE))) {
    stop("At least one of body, title, or footnote needs a height")
  }

  for (surface in names(heights)) {
    heights[[surface]] <- check_row_height_(
      heights[[surface]],
      surface,
      unit
    )
  }

  x$clinify_config$row_height <- c(heights, list(rule = rule))
  x
}

#' Check one row height and convert it to inches
#'
#' flextable measures in inches, centimetres or millimetres, so a pitch given
#' in points is converted on the way in.
#'
#' @param height The height as given
#' @param surface Which of body, title or footnote this is, for the message
#' @param unit The unit the height was given in
#'
#' @return The height in inches, or NULL
#'
#' @noRd
check_row_height_ <- function(height, surface, unit) {
  if (is.null(height)) {
    return(NULL)
  }

  if (
    !is.numeric(height) ||
      length(height) != 1 ||
      is.na(height) ||
      height <= 0
  ) {
    stop(
      "`",
      surface,
      "` must be a single positive number of ",
      unit,
      ", not ",
      paste(deparse(height), collapse = "")
    )
  }

  height /
    switch(
      unit,
      pt = 72,
      "in" = 1,
      cm = 2.54,
      mm = 25.4
    )
}

#' The configured row height for one surface
#'
#' @param config A clinify_config
#' @param surface One of body, title, or footnote
#'
#' @return A list with the height and rule, or NULL if none was configured
#'
#' @noRd
row_height_for_ <- function(config, surface) {
  cfg <- config$row_height

  if (is.null(cfg) || is.null(cfg[[surface]])) {
    return(NULL)
  }

  list(height = cfg[[surface]], rule = cfg$rule)
}

#' Apply a row pitch to every row of a table part
#'
#' @param ft A flextable object
#' @param height The pitch, in inches, or NULL to leave the part alone
#' @param part The part to apply it to
#'
#' @return A flextable object
#'
#' @noRd
apply_row_height_ <- function(ft, height, part = "body") {
  if (is.null(height)) {
    return(ft)
  }

  ft <- flextable::hrule(ft, rule = height$rule, part = part)
  flextable::height_all(ft, height = height$height, part = part)
}

#' Apply a row pitch to a single row of a table part
#'
#' Used for the group label and caption rows, which clinify inserts as it
#' renders and which take the body pitch.
#'
#' @param ft A flextable object
#' @param i The row to apply it to
#' @param height The pitch, in inches, or NULL to leave the row alone
#' @param part The part the row is in
#'
#' @return A flextable object
#'
#' @noRd
apply_row_height_at_ <- function(ft, i, height, part) {
  if (is.null(height)) {
    return(ft)
  }

  ft <- flextable::hrule(ft, i = i, rule = height$rule, part = part)
  flextable::height(ft, i = i, height = height$height, part = part)
}

#' Apply the table's own configuration over its styling
#'
#' The default styling function is where an organisation's house style lives,
#' so whatever the table itself was configured with is applied over the top of
#' it - a per table setting beats the house style, which beats the flextable
#' default. Kept separate from the styling function so that it still runs when
#' the styling is skipped.
#'
#' @param x A clintable object
#'
#' @return A clintable object
#'
#' @noRd
finish_table_ <- function(x) {
  x <- table_align_(x)
  x <- apply_header_pad_(x)
  x <- apply_spanner_rule_(x)
  # Covers a table that is never sliced into pages. A paginated one gets the
  # same gap put on the first row of each of its pages as they are built
  x <- apply_rule_to_body_(x, x$clinify_config$header_pad)
  apply_row_height_(x, row_height_for_(x$clinify_config, "body"))
}

#' Style a title block
#'
#' @param ft The title flextable
#' @param config The clinify_config holding the row height
#'
#' @return A flextable object
#'
#' @noRd
style_titles_ <- function(ft, config) {
  ft <- getOption("clinify_titles_default")(ft)
  apply_row_height_(ft, row_height_for_(config, "title"))
}

#' Style a footnote block, or a footnote page
#'
#' @param ft The footnote flextable
#' @param config The clinify_config holding the row height
#'
#' @return A flextable object
#'
#' @noRd
style_footnotes_ <- function(ft, config) {
  ft <- getOption("clinify_footnotes_default")(ft)
  apply_row_height_(ft, row_height_for_(config, "footnote"))
}
