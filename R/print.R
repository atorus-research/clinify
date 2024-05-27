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

  if (get_pagination_method(x) == "default") {
    nrows <- max(c(nrows, nrow(refdat)))
    pg <- slice_clintable(ct, 1:nrows, eval_select(x$col_keys, refdat))
    print_clinpage(pg)
  }

  # TODO: Write the alternating page object print method
}

#' Print a clinpage object
#'
#' @param x A clinpage object
#'
#' @return Invisible
#' @export
#'
#' @examples
#'
print_clinpage <- function(x) {
  x <- flextable::htmltools_value(x = x)
  x[[3]] <- gsub("(?<!th)  ", "&nbsp; ", x[[3]], perl=TRUE)
  x[[3]] <- gsub('(<span\\b[^>]*>) ', '\\1&nbsp;', x[[3]], perl=TRUE)
  htmltools::browsable(x)
}
