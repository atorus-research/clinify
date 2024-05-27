#' Create a new clintable object
#'
#' A clintable object directly inherits from a flextable object. This function
#' will pass all necessary parameters `flextable::flextable()` and conver the
#' object to a `clintable`
#'
#' @param data A data frame
#' @param ... Parameters to pass to `flextable::flextable()`
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' clintable(mtcars)
clintable <- function(data, ...) {
    as_clintable(flextable(data, ...))
}

#' Convert a flextable into a clintable object
#'
#' @param ft A flextable object
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' ft <- flextable(mtcars)
#' as_clintable(ft)
as_clintable <- function(x) {
    stopifnot(inherits(x, "flextable"))

    # Tack in clinify configurations
    x$clinify_config <- list(
        pagination_method = "default"
    )

    class(x) <- c("clintable", "flextable")
    x
}

#' Blank object creator for a clintable object
#'
#' @noRd
new_clintable <- function() {
    structure(
        list(
            header = NULL,
            body = NULL,
            footer = NULL,
            col_keys = NULL,
            caption = NULL,
            blanks = NULL,
            properties = NULL
        ),
        class = c("clintable", "flextable")
    )
}
