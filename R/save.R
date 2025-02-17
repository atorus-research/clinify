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
write_clintable <- function(x, settings = getOption('clin_doc_settings'), ...) {
  refdat <- x$body$dataset
  pg_method <- get_pagination_method(x)

  doc <- read_docx()

  # Settings from options
  settings_ <- list(
    page_size = page_size(width=11.7, height = 8.3, orient = "landscape"),
    page_margins = page_mar(top=1, bottom=1, left=0.5, right=1),
    type = "continuous"
  )

  if (!is.null(settings) {
    for (n in names(settings)) {
      settings_[[n]] <- settings[[n]]
    }
  })

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
    nrows <- min(c(nrows, nrow(refdat)))
    pg <- slice_clintable(x, 1:nrows, eval_select(x$col_keys, refdat))
    print_clinpage(pg, titles, footnotes)
  } else if (pg_method == "alternating") {
    print_alternating(x, n=n)
  }

}

#' Print a clinpage object
#'
#' @param x A clinpage object
#'
#' @return Invisible
#'
#' @noRd
write_clinpage <- function(x, titles = NULL, footnotes = NULL) {

  body <- flextable::htmltools_value(x = x)
  # Two different type of leading spaces that appear in the HTML
  body[[3]] <- gsub("(?<!th)  ", "&nbsp; ", body[[3]], perl=TRUE)
  body[[3]] <- gsub('(<span\\b[^>]*>) ', '\\1&nbsp;', body[[3]], perl=TRUE)
  # Concurrent spaces
  body[[3]] <- gsub("&nbsp;  ", "&nbsp;&nbsp; ", body[[3]], perl=TRUE)

  if (!is.null(titles)) {
    # TODO: This should take into consideration how many cells are merged within the header
    titles <- width(titles, width = flextable_dim(x)$widths / 2)
    hdr <- flextable::htmltools_value(x = titles)[[3]]
    body[[3]] <- htmltools::HTML(paste0(hdr, body[[3]]))
  }

  if (!is.null(footnotes)) {
    footnotes <- width(footnotes, width = flextable_dim(x)$widths / 2)
    ftr <- flextable::htmltools_value(x = footnotes)[[3]]
    body[[3]] <- htmltools::HTML(paste0(body[[3]], ftr))
  }

  out <- htmltools::browsable(body)
  print(out)
  invisible(out)
}

#' Method for printing alternating pages
#'
#' @param x a clintable object
#' @noRd
write_alternating <- function(x, n) {
  refdat <- x$body$dataset

  pag_idx <- x$clinify_config$pagination_idx

  out <- lapply(pag_idx[1:n], \(p) {
    print_clinpage(slice_clintable(x, p$rows, p$cols))
  })

  for (p in out) {
    print(p)
  }

  invisible(out)
}
