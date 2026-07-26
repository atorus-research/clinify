#' Add titles, footnotes, or a footnote page to a clintable or clindoc
#'
#' This function allows you to attach specified titles, footnotes, or a footnote page
#' into clintable or clindoc object. The input can be provided either as a list of character
#' vectors, or pre-built flextable.
#'
#' When using the `ls` parameter, each element of the list can contain no more than two
#' elements within each character vector. In a title, a single element will align center.
#' In a footnote, a single element will align to the left. For both titles and footnotes,
#' two elements will align split down the middle, with the left side element aligning left
#' and the right side element aligning right.
#'
#' Use `align` to place a line somewhere other than its default. A line holding
#' a single element can go `"left"`, `"center"`, or `"right"`; a line holding two
#' elements is split down the middle by construction, which `align` spells
#' `"split"`. `NA` leaves a line where it would have landed anyway.
#'
#' Instead of a list, `ls` can be a data frame holding every line for a table
#' at once, so one object feeds the titles, the footnotes and a footnote page
#' together. Each of the three functions takes the rows that belong to it and
#' ignores the rest, and a surface with no rows is left alone. Rows are used in
#' the order they are given.
#'
#' | column | holds |
#' | --- | --- |
#' | `type` | `"title"`, `"footnote"`, or `"footnote_page"` (plurals accepted) |
#' | `text1` | the line, or its left hand side |
#' | `text2` | the right hand side of a split line, blank or `NA` if there is none |
#' | `align` | as the `align` argument below, blank or `NA` for the default |
#'
#' Only `type` and `text1` are required. Reading the spec in is left to you -
#' it is an ordinary data frame, so it can come from a spreadsheet, a CSV, a
#' database, or be written out by hand.
#'
#' `tokens` fills in `{NAME}` placeholders, which is how a program path or a run
#' date gets into text that was written somewhere else. `{PAGE}` and
#' `{NUMPAGES}` are left alone - those become real Word page number fields when
#' the table renders, so do not pass them as tokens.
#'
#' @param x a clintable object
#' @param ls a list of character vectors, no more than 2 elements to a vector,
#'   or a data frame spec as described above
#' @param ft A flextable object to use as the header
#' @param align Where to place each line, as a character vector holding one
#'   value per element of `ls` (or a single value for all of them). Values are
#'   `"left"`, `"center"`, `"right"`, `"split"`, or `NA` to keep the default for
#'   that line. Cannot be used together with `ft`, or with a spec that already
#'   has an `align` column.
#' @param tokens Replacements for `{NAME}` placeholders in the text, as a named
#'   list or character vector - `tokens = list(FILE = "programs/t14-1-01.R")`
#'   turns `{FILE}` into that path. Cannot be used together with `ft`.
#'
#' @return A clintable object
#'
#' @family add_titles_footnotes
#' @rdname add_titles_footnotes
#'
#' @export
#'
#' @examples
#' clintable(mtcars) |>
#'   clin_add_titles(
#'     list(
#'       c("Left", "Right"),
#'       c("Just the middle")
#'     )
#'   ) |>
#'   clin_add_titles(
#'     list(
#'       c("Protocol: ABC", "Page {PAGE} of {NUMPAGES}"),
#'       "Table 14-2.01",
#'       "Summary of Demographic and Baseline Characteristics"
#'     ),
#'     # the title line stays centered, the one below it goes left
#'     align = c(NA, NA, "left")
#'   ) |>
#'   clin_add_footnotes(
#'     list(
#'       c(
#'         "Here's a footnote.",
#'         format(Sys.time(), "%H:%M %A, %B %d, %Y")
#'       )
#'     )
#'   ) |>
#'   clin_add_footnote_page(
#'     list(
#'       c(
#'         "Use when you have a lot of footnotes",
#'         "And you don't want to put them on every page"
#'       )
#'     )
#'   )
#'
#' # Or keep every line for the table in one place and let each function take
#' # the rows that belong to it
#' spec <- data.frame(
#'   type = c("title", "title", "footnote"),
#'   text1 = c("Protocol: ABC", "Table 14-2.01", "Source: {FILE}"),
#'   text2 = c("Page {PAGE} of {NUMPAGES}", NA, NA),
#'   align = c("split", "center", "left")
#' )
#'
#' clintable(mtcars) |>
#'   clin_add_titles(spec, tokens = list(FILE = "programs/t14-2-01.R")) |>
#'   clin_add_footnotes(spec, tokens = list(FILE = "programs/t14-2-01.R"))
#'
clin_add_titles <- function(
  x,
  ls = NULL,
  ft = NULL,
  align = NULL,
  tokens = NULL
) {
  x <- add_titles_footnotes_(x, "titles", ls, ft, align, tokens)
  x
}

#' @family add_titles_footnotes
#' @rdname add_titles_footnotes
#' @export
clin_add_footnotes <- function(
  x,
  ls = NULL,
  ft = NULL,
  align = NULL,
  tokens = NULL
) {
  x <- add_titles_footnotes_(x, "footnotes", ls, ft, align, tokens)
  x
}

#' @family add_titles_footnotes
#' @rdname add_titles_footnotes
#' @export
clin_add_footnote_page <- function(
  x,
  ls = NULL,
  ft = NULL,
  align = NULL,
  tokens = NULL
) {
  x <- add_titles_footnotes_(x, "footnote_page", ls, ft, align, tokens)
  x
}

#' Single method to apply titles or footnotes
#'
#' Called by clin_add_titles and clin_add_footnotes
#' @noRd
add_titles_footnotes_ <- function(
  x,
  sect,
  ls = NULL,
  ft = NULL,
  align = NULL,
  tokens = NULL
) {
  stopifnot(inherits(x, 'clintable') || inherits(x, 'clindoc'))

  if (all(is.null(ls), is.null(ft)) || all(!is.null(ls), !is.null(ft))) {
    stop("One of, and only one of, ls or ft must be populated")
  }

  if (!is.null(ft) && !is.null(align)) {
    stop("align cannot be used with ft - align a prebuilt flextable directly")
  }

  if (!is.null(ft) && !is.null(tokens)) {
    stop(
      "tokens cannot be used with ft - substitute into a prebuilt flextable ",
      "before passing it"
    )
  }

  # A data frame carries the lines for every surface at once, so the rows for
  # this one are picked out of it
  if (is.data.frame(ls)) {
    spec <- title_lines_from_df_(ls, sect, align)

    # Nothing for this surface is not an error - one data frame is meant to
    # feed titles, footnotes and a footnote page together
    if (length(spec$ls) == 0) {
      return(x)
    }

    ls <- spec$ls
    align <- spec$align
  }

  if (!is.null(ls)) {
    ls <- substitute_tokens_(ls, tokens)
    ft <- new_title_footnote(ls, sect, align)
  }

  x$clinify_config[[sect]] <- ft
  x
}

#' Create a new title or footnote flextable
#'
#' @param x a list of character vectors, no more than 3 elements to a vector.
#' @param sect Either "titles" or "footnotes"
#' @param align Where to place each line - `"left"`, `"center"`, `"right"`,
#'   `"split"`, or `NA` for the default. One value per element of `x`, or a
#'   single value for all of them.
#'
#' @return A flextable object
#' @export
#'
#' @examples
#'
#' title <- new_title_footnote(
#'   list(
#'     # We'll add tools to automate paging
#'     c("Protocol: CDISCPILOT01", "Page {PAGE} of {NUMPAGES}"),
#'     c("Table 14-2.01"),
#'     c("Summary of Demographic and Baseline Characteristics")
#'   ),
#'   "titles"
#' )
#'
#' footnote <- new_title_footnote(
#'   list(
#'     # We'll add tools to automate paging
#'     c("Page {PAGE}", "Total Pages: {NUMPAGES}")
#'   ),
#'   "footnotes"
#' )
#'
new_title_footnote <- function(
  x,
  sect = c("titles", "footnotes", "footnote_page"),
  align = NULL
) {
  sect <- match.arg(sect)

  lens <- vapply(x, length, integer(1))

  # Check if all lists have length <=3
  if (any(lens > 2)) {
    stop("All sublists must have length <= 2")
  }

  if (any(lens < 1)) {
    stop(
      "Every title or footnote line needs at least one element. Use \"\" for ",
      "a blank line. Empty: ",
      paste(which(lens < 1), collapse = ", ")
    )
  }

  align <- resolve_line_align_(align, lens, sect)

  # List to hold all data frames
  dfs <- list()

  for (i in seq_along(x)) {
    elements <- x[[i]]

    # Create a vector of length 3 filled with the last element
    row <- rep(tail(elements, 1), 2)
    # Replace the first n elements of the vector with the elements of the list
    row[seq_along(elements)] <- elements

    # Create a dataframe with 1 row and 2 columns
    df <- data.frame(t(row))
    names(df) <- c("Left", "Right")

    # Add the data frame to the list
    dfs[[i]] <- df
  }

  # Combine all data frames into one
  df <- dplyr::bind_rows(dfs)

  # Convert the data frame to a flextable
  ft <- flextable::flextable(df)

  # Apply the common styling
  ft <- ft |>
    flextable::set_header_labels(Left = "", Right = "") |>
    flextable::delete_part(part = "header") |>
    flextable::delete_part(part = "footer")

  # Apply different styling based on the number of elements
  for (i in seq_along(x)) {
    if (lens[i] == 1) {
      # Both cells hold the same text, so merging gives the line the full
      # width and the merged cell takes the first cell's alignment
      ft <- ft |>
        flextable::merge_h(i = i, part = "body") |>
        flextable::align(j = 1, i = i, align = align[i], part = "body")
    } else {
      ft <- ft |>
        flextable::merge_h(i = i, part = "body") |>
        flextable::align(j = 1, i = i, align = "left", part = "body") |>
        flextable::align(j = 2, i = i, align = "right", part = "body")
    }
  }

  # Return the flextable
  return(ft)
}

#' Work out the alignment of each title or footnote line
#'
#' A line holding one element takes the whole width, so it can be placed
#' anywhere across the page. A line holding two elements is split down the
#' middle by construction, and "split" is the only thing `align` can say
#' about it.
#'
#' @param align The `align` argument as the user gave it
#' @param lens Number of elements on each line
#' @param sect Either "titles", "footnotes", or "footnote_page"
#'
#' @return A character vector of alignments, one per line
#'
#' @noRd
resolve_line_align_ <- function(align, lens, sect) {
  # Titles centre a lone line and footnotes send it left
  default <- if (sect == "titles") "center" else "left"
  out <- ifelse(lens == 1, default, "split")

  if (is.null(align)) {
    return(out)
  }

  if (!is.character(align) || !length(align) %in% c(1, length(lens))) {
    stop(
      "align must be a character vector of length 1 or ",
      length(lens),
      " (one per title or footnote line)"
    )
  }

  align <- rep_len(align, length(lens))

  known <- align %in% c("left", "center", "right", "split") | is.na(align)
  if (!all(known)) {
    stop(
      "align must be one of \"left\", \"center\", \"right\", or \"split\". ",
      "Not recognized: ",
      paste(unique(align[!known]), collapse = ", ")
    )
  }

  # A two element line is split down the middle, and nothing else fits
  clash <- lens == 2 & !is.na(align) & align != "split"
  if (any(clash)) {
    stop(
      "A title or footnote line holding two elements is always split down ",
      "the middle. Give it one element to place it with align, or use ",
      "\"split\". Line(s): ",
      paste(which(clash), collapse = ", ")
    )
  }

  # There is nothing to split when a line holds one element
  clash <- lens == 1 & !is.na(align) & align == "split"
  if (any(clash)) {
    stop(
      "A title or footnote line holding one element cannot be split. Give it ",
      "two elements, or align it left, center, or right. Line(s): ",
      paste(which(clash), collapse = ", ")
    )
  }

  # NA leaves a line on the default for its shape
  keep <- !is.na(align) & lens == 1
  out[keep] <- align[keep]
  out
}

#' Which values of a spec's `type` column belong to which surface
#' @noRd
title_types_ <- list(
  titles = c("title", "titles"),
  footnotes = c("footnote", "footnotes"),
  footnote_page = c("footnote_page", "footnote_pages")
)

#' Pull one surface's lines out of a title and footnote spec
#'
#' The spec holds every line for a table in one long data frame, so that a
#' single object can feed the titles, the footnotes and a footnote page. Rows
#' are taken in the order they are given.
#'
#' @param spec A data frame with a `type` column, a `text1` column, and
#'   optionally `text2` and `align`
#' @param sect Either "titles", "footnotes", or "footnote_page"
#' @param align The `align` argument, which cannot be combined with an
#'   `align` column
#'
#' @return A list with the lines for this surface and their alignment
#'
#' @noRd
title_lines_from_df_ <- function(spec, sect, align = NULL) {
  if (!"type" %in% names(spec)) {
    stop(
      "A title and footnote spec needs a `type` column saying which lines are ",
      "titles, which are footnotes, and which belong on a footnote page"
    )
  }

  if (!"text1" %in% names(spec)) {
    stop("A title and footnote spec needs a `text1` column holding the text")
  }

  known <- unlist(title_types_, use.names = FALSE)
  types <- tolower(trimws(as.character(spec$type)))
  unknown <- setdiff(unique(types), known)

  if (length(unknown) > 0) {
    stop(
      "The `type` column can hold ",
      paste(sQuote(known), collapse = ", "),
      ". Not recognized: ",
      paste(sQuote(unknown), collapse = ", ")
    )
  }

  if (!is.null(align) && "align" %in% names(spec)) {
    stop(
      "align cannot be used when the spec already has an `align` column - ",
      "drop one of them"
    )
  }

  rows <- which(types %in% title_types_[[sect]])

  if (length(rows) == 0) {
    return(list(ls = list(), align = NULL))
  }

  text1 <- as.character(spec$text1)[rows]
  text2 <- if ("text2" %in% names(spec)) {
    as.character(spec$text2)[rows]
  } else {
    rep(NA_character_, length(rows))
  }

  # A line is one element unless it has a second piece of text to sit against
  ls <- lapply(seq_along(rows), \(i) {
    if (is.na(text2[i]) || !nzchar(text2[i])) {
      text1[i]
    } else {
      c(text1[i], text2[i])
    }
  })

  if ("align" %in% names(spec)) {
    align <- as.character(spec$align)[rows]
    # A blank cell means the line keeps the default for its shape
    align[!nzchar(trimws(align)) & !is.na(align)] <- NA_character_
  }

  list(ls = ls, align = align)
}

#' Replace `{NAME}` placeholders in title and footnote text
#'
#' `{PAGE}` and `{NUMPAGES}` are deliberately left alone - those become real
#' Word page number fields when the table renders.
#'
#' @param ls A list of character vectors of title or footnote text
#' @param tokens A named list or character vector of replacements
#'
#' @return The text with the placeholders replaced
#'
#' @noRd
substitute_tokens_ <- function(ls, tokens) {
  if (is.null(tokens)) {
    return(ls)
  }

  if (
    !(is.list(tokens) || is.character(tokens)) ||
      length(tokens) == 0 ||
      is.null(names(tokens)) ||
      !all(nzchar(names(tokens)))
  ) {
    stop(
      "tokens must be a named list or character vector, like ",
      "list(FILE = \"programs/t14-1-01.R\")"
    )
  }

  values <- vapply(
    tokens,
    \(value) {
      if (length(value) != 1 || is.na(value)) {
        stop("Each token must be a single, non missing value")
      }
      as.character(value)
    },
    character(1)
  )

  lapply(ls, \(line) {
    for (token in names(values)) {
      line <- gsub(
        paste0("{", token, "}"),
        values[[token]],
        line,
        fixed = TRUE
      )
    }
    line
  })
}
