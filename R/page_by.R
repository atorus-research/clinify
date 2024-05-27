#' Configure alternating pages during pagination of a clintable
#'
#' This function configures alternating pages on a clintable object.
#' @param x A clintable object
#' @param key_cols A character vector of variable names
#' @param col_groups A list of character vectors of variable names
#' @param max_rows The maximum number of rows to include on a page
#'
#' @return A clintable object
#' @export
#'
#' @examples
#' ct <- clintable(mtcars)
#'
#' ct <- clin_alt_pages(
#'   ct,
#'   key_cols = c('mpg', 'cyl', 'hp'),
#'   col_groups = list(
#'     c('disp', 'drat', 'wt'),
#'     c('qsec', 'vs', 'am'),
#'     c('gear', 'carb')
#'   ),
#'   max_rows = 10
#' )
clin_alt_pages <- function(x, key_cols, col_groups, max_rows = 10) {

    refdat <- x$body$dataset
    x <- set_pagination_method(x, 'alternating')

    # TODO: Replace this with a method function?
    if (max_rows) {
        tot_rows <- x$body$content$nrow
        page_ends <- which(seq_along(1 : tot_rows) %% max_rows == 0)
        page_starts <- page_ends - max_rows + 1

        if (tot_rows %% max_rows) {
            page_starts[length(page_starts) + 1] <- tail(page_ends, 1) + 1
            page_ends[length(page_ends) + 1] <- tot_rows
        }
        # Create vectors for each row that belongs to a page
        num_page_grps <- length(page_starts)
        page_vecs <- make_page_vecs(page_starts, page_ends)
    }

    key_idx <- eval_select(key_cols, refdat)
    grp_idx <- lapply(col_groups, eval_select, data=refdat)
    col_vecs <- lapply(grp_idx, \(x) append(key_idx, x))

    x$clinify_config$pagination_idx <- make_ind_list(col_vecs, page_vecs)
    x
}

#' Internal method to take a vector of start and end indices and generate the
#' sequence of numbers between each point
#'
#' @param starts from indices
#' @param ends to indices
#'
#' @noRd
make_page_vecs <- function(starts, ends) {
    out <- vector('list', length=length(starts))
    for (i in seq_along(starts)){
        out[[i]] <- starts[i]:ends[i]
    }
    out
}

#' Generate the rows/columns pairing for each output page
#'
#' @param col_vecs list of numeric vectors
#' @param page_vecs list of numeric vectors
#'
#' @return list of n pages with rows and cols elements
#'
#' @noRd
make_ind_list <- function(col_vecs, page_vecs) {
    page_nums <- seq(1, length(col_vecs) * length(page_vecs))
    rows <- unlist(lapply(page_vecs, \(x) rep(list(x), length(col_vecs))), recursive = FALSE)
    cols <- rep(col_vecs, length(page_vecs))
    lapply(page_nums, \(i) list(rows = rows[[i]], cols = cols[[i]]))
}










