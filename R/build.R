### Build Functions

#' Trigger the build of the \code{clin_document}
#'
#' @description The functions used to assemble a \code{clin_document} object
#' do not trigger the processing of any data. Rather, a lazy execution style
#' is used to allow you to construct your table and then explicitly state when
#' the data processing should happen. \code{build} triggers this event.
#'
#' @details When the \code{build} command is executed, all of the data
#' processing commences. Any pre-processing necessary within the table
#' environment takes place first. Next, each of the layers begins executing.
#' Once the layers complete executing, the output of each layer is stacked into
#' the resulting data frame.
#'
#' Once this process is complete, any post-processing necessary within the table
#' environment takes place, and the final output can be delivered.
#'
#' @param x A \code{clin_document} object
#'
#' @return An executed \code{clin_document}
#' @export
#'
#' @examples
#' # Load in Pipe
#' library(magrittr)
#'
#' clin_document(iris) %>% build()
#'
#' @seealso clin_document
build <- function(x) {
    UseMethod("build")
}

#' clin_document S3 method
#' @noRd
#' @export
build.clin_document <- function(x) {

    x <- paginate(x)

    x$pages <- lapply(x$pages, build)
    x
}

#' Process source data to create separate pages
#'
#' This is an internal method, but is exported to support S3 dispatch. Not intended for direct use by a user.
#' @param x a clin_document object
#' @param ... arguments passed to dispatch
#'
#' @return The clin_document object with 'pages' attribute
#' @export
#' @keywords internal
paginate <- function(x, ...) {
    UseMethod("paginate")
}
