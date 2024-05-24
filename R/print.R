#' Clintable print method
#'
#' Extraction of flextable print method with special handling of clintable pages
#' and
#'
#' @param x a clintable object
#' @param n number of pages within the clintable to print
#' @param ... further arguments to be passed to or from other methods. They are ignored in this function.
#'
#' @return Invisible
#' @export
#'
print.clintable <- function(x, n=3, ...) {
    # TODO: Update method to slice and print first n pages
    x <- flextable::htmltools_value(x = x)
    x[[3]] <- gsub("(?<!th)  ", "&nbsp; ", x[[3]], perl=TRUE)
    x[[3]] <- gsub('(<span\\b[^>]*>) ', '\\1&nbsp;', x[[3]], perl=TRUE)
    htmltools::browsable(x)
}
