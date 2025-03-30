#' Convert a Clintable to a Word Document
#'
#' These functions convert a `clintable` object into a Word document using
#' officer and flextable formatting. They apply default styles for titles,
#' footnotes, and table settings while supporting pagination options.
#'
#' - `clindoc()` ensures that the input is a `clintable` and then converts it
#'   to a Word document.
#' - `as_clindoc()` performs the conversion, applying default formatting and
#'   handling pagination if specified.
#'
#' @param x A `clintable` object to be converted.
#' @param apply_defaults Logical, whether to apply default title, footnote,
#'   and table formatting. Defaults to `TRUE`.
#' @param ... Additional arguments (currently unused).
#'
#' @return An `officer::rdocx` object representing the formatted Word document.
#' @export
#' @name clindoc
#'
#' @examples
#'
#' ct <- clintable(mtcars)
#'
#' clindoc(ct)
#'
clindoc <- function(x, ...) {
  stopifnot(inherits(x, "clintable"))
  as_clindoc(x)
}

#' @rdname clindoc
#' @export
as_clindoc <- function(x, apply_defaults = TRUE) {
  pg_method <- x$clinify_config$pagination_method
  titles <- x$clinify_config$titles
  footnotes <- x$clinify_config$footnotes
  footnote_page <- x$clinify_config$footnote_page

  doc <- officer::read_docx()
  settings_ <- getOption("clinify_docx_default")

  if (apply_defaults) {
    if (!is.null(titles)) titles <- getOption("clinify_titles_default")(titles)
    if (!is.null(footnotes)) footnotes <- getOption("clinify_footnotes_default")(footnotes)
    x <- getOption("clinify_table_default")(x)
    if (!is.null(x$clinify_config$footnote_page)) {
      footnote_page <- getOption("clinify_footnotes_default")(footnote_page)
    }
  }

  if (!is.null(titles)) {
    settings_$header_default <- block_list(titles)
  }
  if (!is.null(footnotes)) {
    settings_$footer_default <- block_list(footnotes)
  }

  # Keep with next paging
  if (!is.null(x$clinify_config$auto_page_var)) {
    x <- auto_page_(x)
  }

  # apply settings to doc
  doc <- body_set_default_section(doc, settings_)

  if (!is.null(footnote_page)) {
    doc <- flextable::body_add_flextable(doc, footnote_page)
    doc <- officer::body_add_break(doc)
  }

  # This point down from print method directly ----
  if (pg_method == "default") {
    doc <- flextable::body_add_flextable(doc, x)
  } else if (pg_method == "custom") {
    x <- prep_pagination_(x)
    doc <- doc_alternating(doc, x)
  }

  class(doc) <- c("clindoc", class(doc))
  doc
}

#' Method for printing alternating pages
#'
#' @param x a clintable object
#' @noRd
doc_alternating <- function(doc, x) {
  pag_idx <- x$clinify_config$pagination_idx
  n <- length(pag_idx)

  # Page breaks up to last page
  for (p in pag_idx[1:n - 1]) {
    doc <- add_table_(doc, x, p)
    doc <- officer::body_add_break(doc)
  }

  # Write last page
  doc <- add_table_(doc, x, pag_idx[[n]])
  doc
}

#' Method to add table to document
#'
#' @param x a clintable object
#' @param doc An officer word document object
#' @noRd
add_table_ <- function(doc, x, p) {
  tbl <- slice_clintable(x, p$rows, p$cols)
  if (!is.null(p$label)) {
    tbl <- flextable::add_header_lines(tbl, values = paste(p$label, collapse = "\n"))
    tbl <- flextable::align(tbl, 1, 1, "left", part = "header")
  }

  if (!is.null(p$captions)) {
    # TODO: Allow formatting on this
    tbl <- flextable::add_footer_lines(tbl, values = paste(p$captions, collapse = "\n"))
    # This has to be applied after the footer is added
    tbl <- getOption("clinify_caption_default")(tbl)
  }
  doc <- flextable::body_add_flextable(doc, tbl)
}
