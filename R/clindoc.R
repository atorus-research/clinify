#' Create a `clindoc` object
#'
#' These functions handle the conversion of a `clintable` object into a `clindoc`
#' object.
#'
#' - `as_clindoc()` is intended for a single clintable object and extracts all
#'   necessary title and footnote information to apply to the document
#' - `clindoc()` can accept `clintable` objects as separate parameters or as a
#'   list of `clintable` objects. If a single `clintable` is passed, this function
#'   simply calls `as_clindoc()` internally.
#'
#' When a multiple `clintable` objects are passed to `clindoc()`, titles and footnotes
#' should be applied directly to the `clindoc` object using `clin_add_title()`,
#' `clin_add_footnote()`, or `clin_add_footnote_page()`. Title and footnote information
#' on the individual `clintable` objects will be ignored.
#'
#' @param x A `clintable` object to be converted.
#' @param ... `clintable` objects to be converted. Or separately, a list of `clintable` objects
#'
#' @return a `clindoc` object, inherited from an `officer::rdocx` object
#' @export
#' @name clindoc
#'
#' @examples
#'
#' ct <- clintable(mtcars)
#'
#' clindoc(ct)
#'
clindoc <- function(...) {
  tabs <- list(...)

  if (inherits(tabs[[1]], 'list')) {
    tabs <- tabs[[1]]
  }

  for (x in tabs) {
    stopifnot(inherits(x, "clintable"))
  }

  # If it's just a clintable then convert to doc
  if (length(tabs) == 1) {
    doc <- as_clindoc(tabs[[1]])
  } else {
    doc <- new_clindoc()

    # Tables with page breaks separating
    for (x in tabs[-length(tabs)]) {
      doc <- add_clintable_(doc, x)
      ctx <- officer::body_append_start_context(doc)
      officer::write_elements_to_context(
        context = ctx,
        officer::fpar(officer::run_pagebreak())
      )
      doc <- officer::body_append_stop_context(ctx)
    }

    # Add last so no page break follows
    doc <- add_clintable_(doc, tabs[[length(tabs)]])
  }

  doc
}

#' Object generator for clindocs
#'
#' @param settings Objects for titles, footnotes, or footnote page
#'
#' @noRd
new_clindoc <- function() {
  doc <- officer::read_docx()
  doc$clinify_config <- NULL
  class(doc) <- c("clindoc", class(doc))
  doc
}

#' @rdname clindoc
#' @export
as_clindoc <- function(x) {
  footnote_page <- x$clinify_config$footnote_page

  doc <- new_clindoc()
  doc$clinify_config$titles <- x$clinify_config$titles
  doc$clinify_config$footnotes <- x$clinify_config$footnotes

  if (!is.null(footnote_page)) {
    footnote_page <- getOption("clinify_footnotes_default")(footnote_page)
    doc <- flextable::body_add_flextable(doc, footnote_page)
    doc <- officer::body_add_break(doc)
  }

  doc <- add_clintable_(doc, x)

  doc
}

#' Add a clintable into a clindoc object
#'
#' @param x a clintable object
#' @noRd
add_clintable_ <- function(doc, x) {
  pg_method <- x$clinify_config$pagination_method
  x <- getOption("clinify_table_default")(x)

  # Keep with next paging
  if (!is.null(x$clinify_config$auto_page_var)) {
    x <- auto_page_(x)
  }

  # This point down from print method directly ----
  if (pg_method == "default") {
    doc <- flextable::body_add_flextable(doc, x)
  } else if (pg_method == "custom") {
    x <- prep_pagination_(x)
    doc <- doc_alternating_(doc, x)
  }
}

#' Method for printing alternating pages
#'
#' @param x a clintable object
#' @noRd
doc_alternating_ <- function(doc, x) {
  pag_idx <- x$clinify_config$pagination_idx

  # Page breaks up to last page
  tbs <- unlist(
    lapply(
      pag_idx,
      \(p, x) list(get_table_(x, p), officer::fpar(officer::run_pagebreak())),
      x = x
    ),
    recursive = FALSE
  )

  ctx <- officer::body_append_start_context(doc)
  for (t in tbs[-length(tbs)]) {
    officer::write_elements_to_context(context = ctx, t)
  }
  doc <- officer::body_append_stop_context(ctx)
  doc
}


#' Method to add table to document
#'
#' @param x a clintable object
#' @param doc An officer word document object
#' @noRd
get_table_ <- function(x, p) {
  tbl <- slice_clintable(x, p$rows, p$cols)
  if (!is.null(p$label)) {
    tbl <- flextable::add_header_lines(
      tbl,
      values = paste(p$label, collapse = "\n")
    )
    tbl <- getOption("clinify_grouplabel_default")(tbl)
    tbl <- flextable::align(tbl, 1, 1, "left", part = "header")
  }

  if (!is.null(p$captions)) {
    tbl <- flextable::add_footer_lines(
      tbl,
      values = paste(p$captions, collapse = "\n")
    )
    # This has to be applied after the footer is added
    tbl <- getOption("clinify_caption_default")(tbl)
  }

  tbl
}
