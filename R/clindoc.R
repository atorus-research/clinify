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
    doc <- add_clintables_(new_clindoc(), tabs)
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
  doc$clinify_config$row_height <- x$clinify_config$row_height

  if (!is.null(footnote_page)) {
    footnote_page <- style_footnotes_(footnote_page, x$clinify_config)
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
  add_clintables_(doc, list(x))
}

#' Add clintables into a clindoc object, separated by page breaks
#'
#' officer's append context serializes the whole document body on start and
#' re-parses it on stop, so each round trip costs the size of the document so
#' far. Opening one per table made building a document quadratic in the number
#' of tables. Instead, buffer everything and write it through a single context,
#' only flushing when a table has to go through `body_add_flextable()`.
#'
#' @param doc a clindoc object
#' @param tabs a list of clintable objects
#' @noRd
add_clintables_ <- function(doc, tabs) {
  pending <- list()

  flush <- function(doc) {
    if (length(pending) == 0) {
      return(doc)
    }
    ctx <- officer::body_append_start_context(doc)
    for (el in pending) {
      officer::write_elements_to_context(context = ctx, el)
    }
    pending <<- list()
    officer::body_append_stop_context(ctx)
  }

  for (i in seq_along(tabs)) {
    x <- tabs[[i]]
    pg_method <- x$clinify_config$pagination_method
    x <- prep_clintable_(x)
    last <- i == length(tabs)

    if (pg_method == "default") {
      doc <- flush(doc)
      doc <- flextable::body_add_flextable(doc, x)
      if (!last) {
        doc <- officer::body_add_fpar(doc, page_break_())
      }
    } else if (pg_method == "custom") {
      pending <- c(pending, page_elements_(x))
      if (!last) {
        pending <- c(pending, list(page_break_()))
      }
    }
  }

  flush(doc)
}

#' Style a clintable and compute its pagination ahead of writing
#'
#' @param x a clintable object
#' @noRd
prep_clintable_ <- function(x) {
  pg_method <- x$clinify_config$pagination_method
  x <- getOption("clinify_table_default")(x)
  # After the styling function, so that what the table itself was configured
  # with holds even if that function set a house style of its own
  x <- finish_table_(x)

  # Keep with next paging
  if (!is.null(x$clinify_config$auto_page_var)) {
    x <- auto_page_(x)
  }

  if (pg_method == "custom") {
    x <- prep_pagination_(x)
  }
  x
}

#' Page sliced tables separated by page breaks, with none after the last
#'
#' @param x a clintable object with pagination prepped
#' @noRd
page_elements_ <- function(x) {
  tbs <- unlist(
    lapply(
      x$clinify_config$pagination_idx,
      \(p) list(get_table_(x, p), page_break_())
    ),
    recursive = FALSE
  )
  tbs[-length(tbs)]
}

#' @noRd
page_break_ <- function() {
  officer::fpar(officer::run_pagebreak())
}


#' Method to add table to document
#'
#' @param x a clintable object
#' @param doc An officer word document object
#' @noRd
get_table_ <- function(x, p) {
  tbl <- slice_clintable(x, p$rows, p$cols)

  # Rows added while rendering take the body pitch
  pitch <- row_height_for_(x$clinify_config, "body")

  # The gap under the header rule comes from the body, so every page needs it
  tbl <- apply_rule_to_body_(tbl, x$clinify_config$header_pad)

  if (!is.null(p$label)) {
    tbl <- flextable::add_header_lines(
      tbl,
      values = paste(p$label, collapse = "\n")
    )
    tbl <- getOption("clinify_grouplabel_default")(tbl)
    tbl <- flextable::align(tbl, 1, 1, "left", part = "header")
    tbl <- apply_row_height_at_(tbl, 1, pitch, "header")
  }

  if (!is.null(p$captions)) {
    tbl <- flextable::add_footer_lines(
      tbl,
      values = paste(p$captions, collapse = "\n")
    )
    # This has to be applied after the footer is added
    tbl <- getOption("clinify_caption_default")(tbl)
    tbl <- apply_row_height_at_(tbl, 1, pitch, "footer")
  }

  tbl
}
