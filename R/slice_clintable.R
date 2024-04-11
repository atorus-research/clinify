new_clintable <- function() {
    structure(
        list(
            header = NULL,
            body = NULL,
            footer = NULL,
            col_keys = NULL,
            caption = NULL,
            blanks = NULL,
            properties = NULL
        ),
        class = c("clintable", "flextable")
    )
}


slice_clintable <- function(x, rows, columns) {
    out <- new_clintable()
    out$header <- slice_complex_tabpart(x$header, 1:nrow(x$header$dataset), columns)
    out$footer <- slice_complex_tabpart(x$footer, 1:nrow(x$footer$dataset), columns)
    # out$footer <- x$footer
    out$body <- slice_complex_tabpart(x$body, rows, columns)
    out$col_keys <- x$col_keys[columns]
    out$caption <- x$caption
    out$blanks <- x$blanks
    out$properties <- x$properties
    out
}

slice_complex_tabpart <- function(x, rows, columns) {

    if (nrow(x$dataset) == 0) {
        rows <- numeric()
    }

    dataset <- x$dataset[rows, columns]

    # Content element
    content <- slice_chunkset_struct(x$content, rows, columns)

    col_keys <- x$col_keys[columns]
    colwidths <- x$colwidths[columns]
    rowheights <- x$rowheights[rows]
    hrule <- x$hrule[rows]

    # Spans
    spans <- x$spans
    spans$rows <- spans$rows[rows,columns]
    spans$columns <- spans$columns[rows,columns]

    # Styles
    styles <- list(cells=NULL, pars=NULL, text=NULL)
    styles$cells <- lapply(x$styles$cells,
                           slice_fpstruct,
                           rows=rows,
                           columns=columns)
    styles$pars <- lapply(x$styles$pars,
                           slice_fpstruct,
                           rows=rows,
                           columns=columns)
    styles$text <- lapply(x$styles$text,
                          slice_fpstruct,
                          rows=rows,
                          columns=columns)


    structure(
        list(
            dataset = dataset,
            content = content,
            col_keys = col_keys,
            colwidths = colwidths,
            rowheights = rowheights,
            hrule = hrule,
            spans = spans,
            styles = styles
        ),
        class="complex_tabpart"
    )
}

slice_fpstruct <- function(x, rows, columns) {
    out <- x
    out$data <- out$data[rows, columns]
    out$keys <- out$keys[columns]
    out$nrow <- length(rows)
    out$ncol <- length(columns)
    out
}

# This is identical to fpstruct for now, but let's see
# if differences pop up
slice_chunkset_struct <- function(x, rows, columns) {
    out <- x
    out$data <- out$data[rows, columns]
    out$keys <- out$content$keys[columns]
    out$nrow <- length(rows)
    out$ncol <- length(columns)
    out
}

y <- slice_clintable(t_1, 1:5, c(1, 3:4))
