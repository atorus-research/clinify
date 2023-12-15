### Page Constructor


#' Create a Clinify page object
#'
#' The \code{clin_page} object is the building block of Clinify clin_document. Page contains titles,
#' footnotes, data in the form of the data.frame, as well as page and table attributes.
#' Upon building the page, a flextable object will be created that can then be saved as
#' .docx file.
#'
#' @details
#' When a \code{clin_page} is created, it will contain the following attributes:
#' \itemize{
#' \item{target - Dataset that will be formatted}
#' \item{headers - Vector containing table headers. This defaults to the target dataset's column names}
#' \item{titles - List of strings that will be displayed at the top of each page}
#' \item{footnotes - List of strings that will be displayed at the bottom of each page}
#' \item{internal_footnotes - List of strings that will be displayed at the bottom of the table, above footnotes}
#' \item{style - A style that will be applied to the output document}
#' }
#'
#' \code{clin_page} allows you a basic interface to instantiate the object. Modifier functions are available to change
#' individual parameters.
#'
#' @param target Dataset that will be placed on a page
#' @param headers List of strings that will be used to name columns
#' @param titles List of strings that will be displayed on top of page
#' @param footnotes List of strings that will be displayed at the bottom of the page
#' @param internal_footnotes List of strings that will be displayed at the bottom of the table, above footnotes
#' @param style Style that will be applied to the page contents
#'
#' @return A \code{clin_page} object
#' @export
#'
#' @examples
#'
#' tab <- clin_page(iris)
#'
clin_page <- function(target,
                      headers=colnames(target),
                      titles=NULL,
                      footnotes=NULL,
                      internal_footnotes=NULL,
                      group_cols=NULL,
                      title_style=NULL,
                      table_style=NULL,
                      footnote_style=NULL) {

    new_clin_page(target,
                  headers=headers,
                  titles=titles,
                  footnotes=footnotes,
                  internal_footnotes=internal_footnotes,
                  group_cols=group_cols,
                  title_style=title_style,
                  table_style=table_style,
                  footnote_style=footnote_style)
}

#' Construct new clin_page
#'
#' @inheritParams clin_page
#' @noRd
new_clin_page <- function(target,
                          headers=colnames(target),
                          titles=NULL,
                          footnotes=NULL,
                          group_cols=NULL,
                          internal_footnotes=NULL,
                          title_style=NULL,
                          table_style=NULL,
                          footnote_style=NULL) {

    # styles are functions that are applied to flextable objects to alter it's appearance
    # default to built-in styles in case nothing has been supplied at the init stage
    if (is.null(title_style)) {title_style <- header_style_default}
    if (is.null(table_style)) {table_style <- table_style_default}
    if (is.null(footnote_style)) {footnote_style <- footer_style_default}

    validate_clin_page(target, headers, titles, footnotes, group_cols,
                       title_style, table_style, footnote_style)

    # Create `clin_page` object
    page_ <- structure(list(
        target = target,
        headers = headers,
        titles = titles,
        footnotes = footnotes,
        internal_footnotes=internal_footnotes,
        group_cols = group_cols,
        title_style=title_style,
        table_style=table_style,
        footnote_style=footnote_style
    ), class = c("clin_page", "list"))

    page_
}

#' Validate clin_page input parameters
#'
#' Most validation is done in the binding functions to reduce code duplication
#'
#' @param target target dataset passed from new_clin_page
#' @param headers List of strings that will be used to name columns
#' @param titles List of strings that will be displayed on top of each page
#' @param footnotes List of strings that will be displayed at the bottom of each page
#' @param group_cols List of column names that indicates which rows should be displayed on the same page
#' @param style Style that will be applied to the output page
#'
#' @noRd
validate_clin_page <- function(target, headers, titles, footnotes, group_cols,
                               title_style, table_style, footnote_style) {

    #TODO: valudate all the clin_page arguments
    # table should be a data.frame
    assertthat::assert_that(inherits(target, "data.frame"),
                            msg = paste0("'target' argument passed to clin_page must be a data.frame,",
                                         "\n", "instead a class of: '", class(target), "' was passed."))
}
