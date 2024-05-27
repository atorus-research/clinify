### Build Functions

#' Trigger the build of the `clin_document`
#'
#' @description The functions used to assemble a `clin_document` object
#' do not trigger the processing of any data. Rather, a lazy execution style
#' is used to allow you to construct your table and then explicitly state when
#' the data processing should happen. `build` triggers this event.
#'
#' @details When the `build` command is executed, all of the data
#' processing commences. Any pre-processing necessary within the table
#' environment takes place first. Next, each of the layers begins executing.
#' Once the layers complete executing, the output of each layer is stacked into
#' the resulting data frame.
#'
#' Once this process is complete, any post-processing necessary within the table
#' environment takes place, and the final output can be delivered.
#'
#' @param x A `clin_document` object
#'
#' @return An executed `clin_document`
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

    # this creates 'pages' attribute for the document, separating the initial
    # dataframe into one or more pages depending on selected method of pagination
    x <- paginate(x)
    x <- build_titles_footnotes(x)
    # now that data for pages has been prepared, create flextable objects for each
    # clin_page and save it as 'output' attribute of clin_page
    x$pages <- lapply(x$pages, build)

    # return the document with built out pages
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

# a helper function to create  flextable objects for titles/footnotes
#' @noRd
build_titles_footnotes<- function(x){

    # create flextable objects for titles and footnotes and apply appropriate styles to them
    if (!is.null(x$titles)){x$titles_rendered <- add_page_header(x$titles) %>% x$title_style()}
    if (!is.null(x$footnotes)){x$footnotes_rendered <- add_page_footer(x$footnotes) %>% x$footnote_style()}

    x
}
