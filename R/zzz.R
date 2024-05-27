#' @import flextable
#' @import officer
#' @importFrom htmltools browsable
#' @importFrom dplyr bind_rows
#' @importFrom magrittr `%>%`
NULL

.onLoad <- function(libname, pkgname) {
    # Ensure that flextable and officer are loaded with clinify
    require(flextable)
    require(officer)
}
