#' Create pages for the document
#'
#' @noRd
#' @export
paginate.clin_document <- function(document) {

    pagination_mode <- determine_pagination_method(document)
    #TODO: implement pagination logic based on pagination mode
    if(pagination_mode=='simple'){
        pages <- list(clin_page(document$target,
                                headers=document$headers,
                                titles=document$titles,
                                footnotes=document$footnotes,
                                internal_footnotes=NULL,
                                group_cols=document$group_cols,
                                title_style=document$title_style,
                                table_style=document$table_style,
                                footnote_style=document$footnote_style)
                      )
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

#' Build individual page
#'
#' @noRd
#' @export
build.clin_page <- function(x){

    #TODO: complete the function by dealing with titles, footnotes, styles

    # create flextable object for the main table that will be displayed on the page
    x$output <- flextable::as_flextable(x$target) %>% x$table_style()

    # create flextable objects for titles and footnotes and apply appropriate styles to them
    if (!is.null(x$titles)){x$titles_rendered <- add_page_header(x$titles) %>% x$title_style()}
    if (!is.null(x$footnotes)){x$footnotes_rendered <- add_page_footer(x$footnotes) %>% x$footnote_style()}

    x
}
