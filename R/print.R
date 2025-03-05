#' Clintable print method
#'
#' Extraction of flextable print method with special handling of clintable pages
#' and
#'
#' @param x A clintable object
#' @param n Number of pages within the clintable to print. Only used when
#'   pagination is configured
#' @param nrows Number of rows to print. Only used when rows aren't configured
#'   within the pagination method
#' @param apply_defaults Apply default styles. These styles are stored in the 
#'   options clinify_header_default, clinify_footer_default, and 
#'   clinify_table_default respectively. Defaults to true. 
#' @param ... Additional parameters passed to flextable print method
#'
#' @return Invisible
#' @export
#' 
#' @family Print methods
#' @rdname print_methods
#' @examples
#'
#' ct <- clintable(mtcars)
#'
#' print(ct)
#'
#' ct <- clin_alt_pages(
#'   ct,
#'   key_cols = c('mpg', 'cyl', 'hp'),
#'   col_groups = list(
#'     c('disp', 'drat', 'wt'),
#'     c('qsec', 'vs', 'am'),
#'     c('gear', 'carb')
#'   )
#' )
#'
#' print(ct)
#'
print.clintable <- function(x, n=3, nrows = 15, apply_defaults=TRUE, ...) {

  out <- clintable_as_html(x, n, nrows, apply_defaults, ...)

  if (interactive()) {
    print(htmltools::browsable(out))
    invisible(out)
  } else {
    out
  }
}

#' @family Print methods
#' @rdname print_methods
#' @export
knit_print.clintable <- function(x, n=3, nrows = 15, apply_defaults=TRUE, ...) {

  out <- clintable_as_html(x, n, nrows, apply_defaults, ...)
  knitr::raw_html(out)
}

#' Render HTML for a clintable
#'
#' Extraction of flextable print method with special handling of clintable pages
#' @noRd
clintable_as_html <- function(x, n=3, nrows = 15, apply_defaults=TRUE, ...) {
  refdat <- x$body$dataset
  pg_method <- x$clinify_config$pagination_method

  titles <- x$clinify_config$titles
  footnotes <- x$clinify_config$footnotes

  # Apply the default styling
  if (apply_defaults) {
    if (!is.null(titles)) titles <- getOption('clinify_titles_default')(titles)
    if (!is.null(footnotes)) footnotes <- getOption('clinify_footnotes_default')(footnotes)
    x <- getOption('clinify_table_default')(x)
  }
  
  if (pg_method == "default") {
    nrows <- min(c(nrows, nrow(refdat)))
    pg <- slice_clintable(x, 1:nrows, eval_select(x$col_keys, refdat))
    out <- print_clinpage(pg, titles, footnotes)
  } else if (pg_method == "custom") {
    x <- prep_pagination_(x)
    out <- print_alternating(x, n=n, titles, footnotes)
  }
  out
}

#' Print a clinpage object
#'
#' @param x A clinpage object
#'
#' @return Invisible
#'
#' @noRd
print_clinpage <- function(x, titles = NULL, footnotes = NULL, group_label = NULL) {

  if (!is.null(group_label)) {
    # TODO: Allow formatting on this
    x <- flextable::add_header_lines(x, values = paste(group_label, collapse="\n"))
    x <- flextable::align(x, 1, 1, 'left', part="header")
  }
  
  body <- flextable::htmltools_value(x = x) 
  # Two different type of leading spaces that appear in the HTML
  body[[3]] <- gsub("(?<!th)  ", "&nbsp; ", body[[3]], perl=TRUE)
  body[[3]] <- gsub('(<span\\b[^>]*>) ', '\\1&nbsp;', body[[3]], perl=TRUE)
  # Concurrent spaces
  body[[3]] <- gsub("&nbsp;  ", "&nbsp;&nbsp; ", body[[3]], perl=TRUE)

  if (!is.null(titles)) {
    # TODO: This should take into consideration how many cells are merged within the header
    titles <- flextable::width(titles, width = flextable::flextable_dim(x)$widths / 2)
    hdr <- flextable::htmltools_value(x = titles)[[3]]
    body[[3]] <- htmltools::HTML(paste0(hdr, body[[3]]))
  }

  if (!is.null(footnotes)) {
    footnotes <- flextable::width(footnotes, width = flextable::flextable_dim(x)$widths / 2)
    ftr <- flextable::htmltools_value(x = footnotes)[[3]]
    body[[3]] <- htmltools::HTML(paste0(body[[3]], ftr))
  }

  body
}

#' Method for printing alternating pages
#'
#' @param x a clintable object
#' @param n number of pages within the clintable to print
#' @noRd
print_alternating <- function(x, n, titles=NULL, footnotes=NULL) {

  pag_idx <- x$clinify_config$pagination_idx

  # Don't try to print more pages than requested
  n <- min(length(pag_idx), n)

  pgs <- lapply(pag_idx[1:n], \(p) {
    print_clinpage(
      slice_clintable(x, p$rows, p$cols),
      titles = titles,
      footnotes = footnotes,
      group_label = p$label)
  })

  # Create the arguments and page selectors for the pages being printed
  args <- list()
  cntrl <- list()
  for (i in seq_along(pgs)) {
    # Generate the divs of the individual pages
    if (i==1) {
      args <- append(args, list(div(id=sprintf("page%s", i), class="page", pgs[i])))
    } else {
      # Only display first page
      args <- append(args, 
        list(div(id=sprintf("page%s", i), style = "display:none;", class="page", pgs[i]))
      )
    }
    
    # Create the buttons for the page selector
    cntrl <- append(cntrl, 
      list(tags$button(
        as.character(i),
        onclick = sprintf("showPage(%s)", i)
      )
    ))
  }

  # Page control div
  pagination_controls <- do.call(div, cntrl)
  # Args of the object
  args <- append(args, list(pagination_controls, pagination_js))

  # Make the taglist and output it
  do.call(tagList, args)
}

#' JS for the page buttons
#' @noRd
pagination_js <- tags$script(HTML("
  function showPage(pageNum) {
    var pages = document.querySelectorAll('.page');
    pages.forEach(page => page.style.display = 'none');
    document.getElementById('page' + pageNum).style.display = 'block';
  }
"))