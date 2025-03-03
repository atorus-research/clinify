#' Create a new clintable object
#'
#' A clintable object directly inherits from a flextable object. This function
#' will pass all necessary parameters `flextable::flextable()` and conver the
#' object to a `clintable`
#'
#' @param x A data frame
#' @param page_by A variable in the input dataframe to use for pagination
#' @param group_by A variable which will be used for grouping and attached 
#'   as a label above the table headers
#' @param use_labels Use variable labels as column headers. Nested levels can be 
#'   achieved using the string "||" as a delimitter. Horizontal and vertical levels 
#'   using identical words will be merged. 
#' @param ... Parameters to pass to `flextable::flextable()`
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' clintable(mtcars)
clintable <- function(x, page_by=NULL, group_by=NULL, use_labels=TRUE, ...) {
    ct <- as_clintable(
        flextable(x, ...), 
        page_by = page_by, 
        group_by = group_by
    )

    if (use_labels) {
      ct <- headers_from_labels_(ct)
    } 
    ct
}

#' Convert a flextable into a clintable object
#'
#' @param ft A flextable object
#' @param page_by A variable in the input dataframe to use for pagination
#' @param group_by A variable which will be used for grouping and attached 
#'   as a label above the table headers
#' 
#' @return A clintable object
#' @export
#'
#' @examples
#' ft <- flextable(mtcars)
#' as_clintable(ft)
as_clintable <- function(x, page_by=NULL, group_by=NULL) {
    stopifnot(inherits(x, "flextable"))

    x$clinify_config$page_by <- page_by
    x$clinify_config$group_by <- group_by

    if (is.null(page_by) & is.null(group_by)) {
        x$clinify_config$pagination_method <- "default"
    } else {
        x$clinify_config$pagination_method <- "custom"
    }

    class(x) <- c("clintable", "flextable")
    x
}

#' Blank object creator for a clintable object
#'
#' @noRd
new_clintable <- function() {
    # TODO: revisit when actual object needs are established
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
