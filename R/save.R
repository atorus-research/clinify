#' Clintable print method
#'
#' Extraction of flextable print method with special handling of clintable pages
#' and
#'
#' @param x a clintable object
#' @param ... Additional parameters passed to flextable print method
#'
#' @return Invisible
#' @export
#'
#' @examples
#'
#' ct <- clintable(mtcars)
#'
#' print(ct)
#'
#' ct <- clin_alt_pages(
#'   ct,
#'   key_cols = c('mpg', 'cyl', 'hp'),
#'   col_groups = list(
#'     c('disp', 'drat', 'wt'),
#'     c('qsec', 'vs', 'am'),
#'     c('gear', 'carb')
#'   ),
#'   max_rows = 10
#' )
#'
#' print(ct)
#'
write_clintable <- function(x, file, settings = getOption('clin_doc_settings'), ...) {
  pg_method <- get_pagination_method(x)

  doc <- read_docx()

  # Settings from options
  settings_ <- list(
    page_size = page_size(width=11.7, height = 8.3, orient = "landscape"),
    page_margins = page_mar(top=1, bottom=1, left=0.5, right=1),
    type = "continuous"
  )

  if (!is.null(settings)) {
    for (n in names(settings)) {
      settings_[[n]] <- settings[[n]]
    }
  }

  if (!is.null(x$clinify_config$titles)) {
    settings_$header_default <- block_list(x$clinify_config$titles)
  }
  if (!is.null(x$clinify_config$footnotes)) {
    settings_$footer_default <- block_list(x$clinify_config$footnotes)
  }

  # apply settings to doc
  doc <- body_set_default_section(doc, do.call(prop_section, settings_))

  # This point down from print method directly ----
  if (pg_method == "default") {
    doc <- body_add_flextable(doc, x)
  } else if (pg_method == "alternating") {
    write_alternating(doc, x)
  }

  print(doc, target=file)
}

#' Print a clinpage object
#'
#' @param x A clinpage object
#'
#' @return Invisible
#'
#' @noRd
# write_clinpage <- function(doc, x) {
#   body_add_flextable(doc, x)
# }

#' Method for printing alternating pages
#'
#' @param x a clintable object
#' @noRd
write_alternating <- function(doc, x) {

  pag_idx <- x$clinify_config$pagination_idx
  n <- length(pag_idx)

  # Page breaks up to last page
  for (p in pag_idx[1:n-1]) {
    doc <- body_add_flextable(doc, slice_clintable(x, p$rows, p$cols))
    doc <- body_add_break(doc)
  }
  # Write last page
  doc <- body_add_flextable(
    doc, 
    slice_clintable(x, pag_idx[[n]]$rows, pag_idx[[n]]$cols)
  )
  doc
}
