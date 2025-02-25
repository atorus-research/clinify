#' Clintable print method
#'
#' Extraction of flextable print method with special handling of clintable pages
#' and
#'
#' @param x a clintable object
#' @param n number of pages within the clintable to print. Only used when
#'   pagination is configured
#' @param nrows number of rows to print. Only used when rows aren't configured
#'   within the pagination method
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
print.clintable <- function(x, n=3, nrows = 15, ...) {

  refdat <- x$body$dataset
  pg_method <- x$clinify_config$pagination_method

  titles <- x$clinify_config$titles
  footnotes <- x$clinify_config$footnotes
  
  if (pg_method == "default") {
    nrows <- min(c(nrows, nrow(refdat)))
    pg <- slice_clintable(x, 1:nrows, eval_select(x$col_keys, refdat))
    print_clinpage(pg, titles, footnotes)
  } else if (pg_method == "custom") {
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
print_clinpage <- function(x, titles = NULL, footnotes = NULL) {

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
#' @param n number of pages within the clintable to print
print_alternating <- function(x, n) {

  pag_idx <- x$clinify_config$pagination_idx

  browser()
  out <- lapply(pag_idx[1:n], \(p) {
    print_clinpage(slice_clintable(x, p$rows, p$cols))
    })

  for (p in out) {
    print(p)
  }

  invisible(out)
}
