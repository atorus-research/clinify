### Document Constructor


#' Create a Clinify document object
#'
#' The `clin_document` object is the main container from which  is constructed. Clinify tables are made up of
#' one or more pages. Document contains titles, footnotes, original data in the form of the data.frame, as well as page and table attributes.
#'
#' @details
#' When a `clin_document` is created, it will contain the following bindings:
#' \itemize{
#' \item{target - Dataset that will be formatted}
#' \item{headers - Vector containing table headers. This defaults to the target dataset's column names}
#' \item{titles - List of strings that will be displayed at the top of each page}
#' \item{footnotes - List of strings that will be displayed at the bottom of each page}
#' \item{maxrows - Maximum amount of rows that would be put on 1 page of the document}
#' \item{group_cols - List of column names that indicates which rows should be displayed on the same page}
#' \item{pagination_strategy - Matrix that describes how a target should be paginated both vertically and horizontally}
#' \item{style - A style that will be applied to the output document`s titles}
#' \item{style - A style that will be applied to the output document`s body(table)}
#' \item{style - A style that will be applied to the output document`s footnotes}
#' }
#'
#' `clin_document` allows you a basic interface to instantiate the object. Modifier functions are available to change
#' individual parameters.
#'
#' @param target Dataset that will be formatted and saved as .docx
#' @param headers List of strings that will be used to name columns
#' @param titles List of strings that will be displayed on top of each page
#' @param footnotes List of strings that will be displayed at the bottom of each page
#' @param maxrows Maximum amount of rows that would be put on 1 page of the document
#' @param group_cols List of column names that indicates which rows should be displayed on the same page
#' @param pagination_strategy Matrix that shows how target should be split to fit on pages
#' @param title_style Style that will be applied to the output document`s titles
#' @param table_style Style that will be applied to the output document` body(table)
#' @param footnote_style Style that will be applied to the output document`s footnotes
#'
#' @return A `clin_document` object
#' @export
#'
#' @examples
#'
#' doc <- clin_document(iris)
#'
clin_document <- function(target,
                          headers=NULL,
                          titles=NULL,
                          footnotes=NULL,
                          maxrows=NULL,
                          group_cols=NULL,
                          pagination_strategy=NULL,
                          title_style=header_style_default,
                          table_style=table_style_default,
                          footnote_style=footer_style_default) {

    new_clin_document(target,
                      headers=headers,
                      titles=titles,
                      footnotes=footnotes,
                      maxrows=maxrows,
                      group_cols=group_cols,
                      pagination_strategy=pagination_strategy,
                      title_style=title_style,
                      table_style=table_style,
                      footnote_style=footnote_style)
    }

#' Construct new clin_document
#'
#' @inheritParams clin_document
#' @noRd
new_clin_document <- function(target,
                              headers=NULL,
                              titles=NULL,
                              footnotes=NULL,
                              maxrows=NULL,
                              group_cols=NULL,
                              pagination_strategy=NULL,
                              title_style=header_style_default,
                              table_style=table_style_default,
                              footnote_style=footer_style_default) {

    validate_clin_document(target, headers, titles, footnotes, maxrows, group_cols,
                           pagination_strategy, title_style, table_style, footnote_style)

    # Create `clin_document` object
    document_ <- structure(list(
        target = target,
        pages = list(),
        headers = headers,
        titles = titles,
        footnotes = footnotes,
        maxrows = maxrows,
        group_cols = group_cols,
        pagination_strategy = pagination_strategy,
        title_style=title_style,
        table_style=table_style,
        footnote_style=footnote_style
    ), class = c("clin_document", "list"))

    document_
}

#' Validate clin_document input parameters
#'
#' Most validation is done in the binding functions to reduce code duplication
#'
#' @param target target dataset passed from new_clin_document
#' @param headers List of strings that will be used to name columns
#' @param titles List of strings that will be displayed on top of each page
#' @param footnotes List of strings that will be displayed at the bottom of each page
#' @param maxrows Maximum amount of rows that would be put on 1 page of the document
#' @param group_cols List of column names that indicates which rows should be displayed on the same page
#' @param pagination_strategy Matrix that shows how target should be split to fit on pages
#' @param title_style Style that will be applied to the output document`s titles
#' @param table_style Style that will be applied to the output document` body(table)
#' @param footnote_style Style that will be applied to the output document`s footnotes
#'
#' @noRd
validate_clin_document <- function(target, headers, titles, footnotes, maxrows, group_cols,
                                   pagination_strategy, title_style, table_style, footnote_style) {

    # table should be a data.frame
    assertthat::assert_that(inherits(target, "data.frame"),
                            msg = paste0("'target' argument passed to clin_document must be a data.frame,",
                                         "\n", "instead a class of: '", class(target), "' was passed."))
}
