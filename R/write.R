#' Clintable write method
#'
#' Write a clinify table out to a docx file
#'
#' @param x a clintable object
#' @param file The file path to which the file should be written
#' @param apply_defaults Apply default styles. These styles are stored in the
#'   options clinify_header_default, clinify_footer_default, and
#'   clinify_table_default respectively. Defaults to true.
#'
#' @return Invisible
#' @export
#'
#' @examples
#' ct <- clintable(mtcars)
#'
#' ct <- clin_alt_pages(
#'   ct,
#'   key_cols = c("mpg", "cyl", "hp"),
#'   col_groups = list(
#'     c("disp", "drat", "wt"),
#'     c("qsec", "vs", "am"),
#'     c("gear", "carb")
#'   )
#' )
#'
#' # Get document object directly
#' doc <- clindoc(ct)
#'
#' # Write out docx file
#' write_clindoc(ct, file.path(tempdir(), "demo.docx"))
#'
write_clindoc <- function(x, file) {
  if (inherits(x, "clindoc")) {
    doc <- x
  } else {
    doc <- as_clindoc(x)
  }

  clinify_config <- doc$clinify_config
  settings <- getOption('clinify_docx_default')

  titles <- doc$clinify_config$titles
  footnotes <- doc$clinify_config$footnotes
  footnote_page <- doc$clinify_config$footnote_page

  # If footnote page applied on doc and not clintable, append to beginning
  if (!is.null(footnote_page)) {
    footnote_page <- getOption("clinify_footnotes_default")(footnote_page)
    doc <- officer::cursor_begin(doc)
    doc <- body_add_flextable(doc, footnote_page, pos = "before")
    # Page break after footnote page
    doc <- body_add_break(doc)
  }

  if (!is.null(titles)) {
    titles <- getOption("clinify_titles_default")(titles)
    settings$header_default <- block_list(titles)
  }
  if (!is.null(footnotes)) {
    footnotes <- getOption("clinify_footnotes_default")(footnotes)
    settings$footer_default <- block_list(footnotes)
  }

  # apply settings to doc
  doc <- body_set_default_section(doc, settings)

  print(doc, target = file)
}
