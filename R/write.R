#' Clintable write method
#'
#' Write a clinify table out to a docx file
#'
#' @param x a clintable object
#' @param file The file path to which the file should be written
#' @param apply_defaults Apply default styles. These styles are stored in the 
#'   options clinify_header_default, clinify_footer_default, and 
#'   clinify_table_default respectively. Defaults to true. 
#' @param settings Settings that will be passed
#'
#' @return Invisible
#' @export
#'
#' @examples
#' \dontrun{
#' ct <- clintable(mtcars)
#'
#' ct <- clin_alt_pages(
#'   ct,
#'   key_cols = c('mpg', 'cyl', 'hp'),
#'   col_groups = list(
#'     c('disp', 'drat', 'wt'),
#'     c('qsec', 'vs', 'am'),
#'     c('gear', 'carb')
#'   )
#' )
#'
#' write_clintable(ct, 'demo.docx')
#' }
write_clintable <- function(x, file, apply_defaults=TRUE) {
  pg_method <- x$clinify_config$pagination_method
  titles <- x$clinify_config$titles
  footnotes <- x$clinify_config$footnotes

  doc <- officer::read_docx()
  settings_ <- getOption('clinify_docx_default')

  if (apply_defaults) {
    if (!is.null(titles)) titles <- getOption('clinify_titles_default')(titles)
    if (!is.null(footnotes)) footnotes <- getOption('clinify_footnotes_default')(footnotes)
    x <- getOption('clinify_table_default')(x)
  }

  if (!is.null(titles)) {
    settings_$header_default <- block_list(titles)
  }
  if (!is.null(footnotes)) {
    settings_$footer_default <- block_list(footnotes)
  }
  
  # apply settings to doc
  doc <- body_set_default_section(doc, settings_)

  # This point down from print method directly ----
  if (pg_method == "default") {
    doc <- body_add_flextable(doc, x)
  } else if (pg_method == "custom") {
    x <- prep_pagination_(x)
    doc <- write_alternating(doc, x)
  }

  print(doc, target=file)
}

#' Method for printing alternating pages
#'
#' @param x a clintable object
#' @noRd
write_alternating <- function(doc, x) {

  pag_idx <- x$clinify_config$pagination_idx
  n <- length(pag_idx)

  # Page breaks up to last page
  for (p in pag_idx[1:n-1]) {
    doc <- add_table_(doc, x, p)
    doc <- body_add_break(doc)
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
    tbl <- add_header_lines(tbl, values = paste(p$label, collapse="\n"))
    tbl<- align(tbl, 1, 1, 'left', part="header")
  }
  doc <- body_add_flextable(doc, tbl)
}