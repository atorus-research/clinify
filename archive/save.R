### Save Functions

#' save the `clin_document` to specified location
#'
#' @description `save` saves the `clin_document` and returns it invisibly.
#'
#' @details By default, `save` method saves the document to the current folder
#' with the name of 'my_doc.docx'
#'
#' @param x A `clin_document` object
#'
#' \item{target - a full path to the document}
#' @export
#'
#' @examples
#' # Load in Pipe
#' library(magrittr)
#'
#' doc <- clin_document(iris) %>% build()
#' save(doc)
#'
#' @seealso clin_document
save <- function(x, ...) {
    UseMethod("save")
}

#' clin_document S3 method
#' @noRd
#' @export
save.clin_document <- function(x, target=NULL) {

    #TODO implement SAVE method properly
    if (is.null(target)) {target = file.path(getwd(), "my_doc.docx")}

    doc <- read_docx()

    # Add all pages to the document
    for (page in x$pages) {
        body_add_flextable(doc, value = page$output)
        }

    # Below part probably can be abstracted as well. Or can be left as is...
    body_set_default_section(doc,
            prop_section(
                page_size = page_size(width=11, height = 8.5, orient = "landscape"),
                page_margins = page_mar(top=0.5, bottom=1, left=1, right=1),
                type = "continuous",
                # here we should pass built flextable objects, but
                # clin_document.titles/footnotes are regular lists
                footer_default = block_list(x$footnotes_rendered),
                header_default = block_list(x$titles_rendered)
                )
    ) %>%

    # Save docs file.
    print(target = target)

    invisible(x)
}

