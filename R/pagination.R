#' Create pages for the document
#'
#' @noRd
#' @export
paginate.clin_document <- function(document) {

    pagination_mode <- determine_pagination_method(document)
    #TODO: implement pagination logic based on pagination mode
    if(pagination_mode=='simple'){
        pages <- list(clin_page(document$target))
        document$pages <- pages
    }
    document
}

#' Determine pagination method
#'
#' @noRd
determine_pagination_method <- function(document) {

    #TODO: write a proper function. Below are proposed methods:
    # 'simple' - when there is only vertical pagination, and there is no need
    # to repeat group headers on different pages
    # 'horizontal' - when horizontal pagination is required
    # 'complex' - when both horizontal and vertical pagination is required

    # pagination method should be determined based on what information has user
    # provided about the document prior to calling build()

    'simple'
}

#' Build pages
#'
#' @noRd
#' @export
build_pages.clin_document <- function(document) {

    sapply(document$pages, build)
    document
}

#' Build individual page
#'
#' @noRd
#' @export
build.clin_page <- function(clin_page){
    #TODO: complete the function by dealing with titles, footnotes, styles
    clin_page$output <- flextable::as_flextable(clin_page$target)
    clin_page
}

