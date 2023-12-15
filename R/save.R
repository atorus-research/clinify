### Save Functions

#' save the \code{clin_document} to specified location
#'
#' @description
#'
#' @details
#'
#' @param x A \code{clin_document} object
#'
#' @return
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
save <- function(x) {
    UseMethod("save")
}

#' clin_document S3 method
#' @noRd
#' @export
save.clin_document <- function(x, target=NULL) {

    #TODO implement SAVE method properly
    if (is.null(target)) {target = file.path(getwd(), "my_doc.docx")}

    doc <- read_docx() %>%
        # Add body table
        body_add_flextable(value = x$pages[[1]]) %>%

        # Below part probably can be abstracted as well. Or can be left as is...
        body_add(
            block_section(
                prop_section(
                    page_size = page_size(orient = "landscape"),
                    page_margins = page_mar(top=0.5, bottom=1, left=1, right=1),
                    type = "continuous",
                    # here we should pass built flextable objects, but
                    # clin_document.titles/footnotes are regular lists
                    footer_default = block_list(x$footnotes),
                    header_default = block_list(x$titles)
                )
            )
        ) %>%

        # Save docs file.
        print(target = target)
}

