#' @import flextable
#' @import officer
#' @importFrom htmltools browsable tags tagList div HTML
#' @importFrom dplyr bind_rows
#' @importFrom magrittr `%>%`
#' @importFrom tidyselect eval_select
NULL

.onLoad <- function(libname, pkgname) {
    # Ensure that flextable and officer are loaded with clinify
    require(flextable)
    require(officer)

    sect <- clinify_docx_default()

    # Save out options to grab defaults
    options(
        clinify_docx_default = sect,
        clinify_titles_default = clinify_titles_default,
        clinify_footnotes_default = clinify_footnotes_default,
        clinify_table_default = clinify_table_default
    )
}
