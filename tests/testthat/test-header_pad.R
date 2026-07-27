# The vertical space around each row, as Word receives it. flextable emits cell
# padding as paragraph spacing rather than as cell margins, so that is where it
# has to be read from
docx_spacing <- function(ct, table = 1) {
  file <- withr::local_tempfile(fileext = ".docx")
  write_clindoc(ct, file = file)

  unzipped <- withr::local_tempdir()
  utils::unzip(file, files = "word/document.xml", exdir = unzipped)
  xml <- xml2::read_xml(file.path(unzipped, "word", "document.xml"))

  rows <- xml2::xml_find_all(
    xml2::xml_find_all(xml, "//w:tbl")[[table]],
    "./w:tr"
  )

  lapply(rows, function(row) {
    cell <- xml2::xml_find_first(row, "./w:tc")
    spacing <- xml2::xml_find_first(cell, "./w:p/w:pPr/w:spacing")
    border <- xml2::xml_find_first(cell, "./w:tcPr/w:tcBorders/w:bottom")

    pt <- function(which) {
      if (inherits(spacing, "xml_missing")) {
        return(NA_real_)
      }
      value <- xml2::xml_attr(spacing, which)
      if (is.na(value)) NA_real_ else as.numeric(value) / 20
    }

    list(
      top = pt("before"),
      bottom = pt("after"),
      rule = if (inherits(border, "xml_missing")) {
        NA_character_
      } else {
        xml2::xml_attr(border, "val")
      }
    )
  })
}

test_that("clin_header_pad records what it was given", {
  ct <- clintable(head(mtcars[, 1:2], 3))

  expect_null(ct$clinify_config$header_pad)

  pad <- clin_header_pad(
    ct,
    above = 18,
    below = 4,
    rule_to_body = 6
  )$clinify_config$header_pad

  expect_equal(pad$above, 18)
  expect_equal(pad$below, 4)
  expect_equal(pad$rule_to_body, 6)

  # Only what was asked for
  partial <- clin_header_pad(ct, above = 18)$clinify_config$header_pad
  expect_equal(partial$above, 18)
  expect_null(partial$below)
  expect_null(partial$rule_to_body)
})

test_that("clin_header_pad is validated", {
  ct <- clintable(head(mtcars[, 1:2], 3))

  expect_error(clin_header_pad(ct), "At least one of above, below")
  expect_error(clin_header_pad(ct, above = "x"), "numbers of points")
  expect_error(clin_header_pad(ct, above = -1), "numbers of points")
  expect_error(clin_header_pad(ct, above = NA), "numbers of points")
  expect_error(clin_header_pad(ct, above = numeric(0)), "numbers of points")
  expect_error(clin_header_pad(ct, below = -1), "`below` must")
  expect_error(clin_header_pad(ct, rule_to_body = -1), "`rule_to_body` must")
  expect_error(clin_header_pad(mtcars, above = 1), "inherits")

  # No space at all is a legitimate thing to ask for
  expect_equal(
    clin_header_pad(ct, above = 0)$clinify_config$header_pad$above,
    0
  )
})

test_that("Header spacing lands where it is named", {
  ct <- clintable(head(mtcars[, 1:2], 3))

  # clinify starts every table with 9 points on both sides of the header
  stock <- docx_spacing(ct)
  expect_equal(stock[[1]]$top, 9)
  expect_equal(stock[[1]]$bottom, 9)

  padded <- docx_spacing(
    clin_header_pad(ct, above = 18, below = 4, rule_to_body = 6)
  )

  # Above the labels, and between the labels and the rule
  expect_equal(padded[[1]]$top, 18)
  expect_equal(padded[[1]]$bottom, 4)

  # And under the rule, which has to come from the body
  expect_equal(padded[[2]]$top, 6)

  # The rule itself is still drawn under the bottom header row
  expect_equal(padded[[1]]$rule, "single")
})

test_that("The rule sits below the header's bottom padding", {
  # This is the whole reason the pieces are named for where they sit: padding
  # under the header moves the rule away from the labels rather than opening
  # space beneath it, so the two are not interchangeable
  ct <- clintable(head(mtcars[, 1:2], 3))

  wide_label_gap <- docx_spacing(clin_header_pad(ct, below = 20))
  wide_body_gap <- docx_spacing(clin_header_pad(ct, rule_to_body = 20))

  # Asking for room under the labels puts it on the header, above the rule
  expect_equal(wide_label_gap[[1]]$bottom, 20)
  expect_equal(wide_label_gap[[2]]$top, 0.1)

  # Asking for room under the rule puts it on the body, below the rule
  expect_equal(wide_body_gap[[1]]$bottom, 9)
  expect_equal(wide_body_gap[[2]]$top, 20)

  # Either way the rule is on the header cell, so it is the header's padding
  # that decides how far from the labels it is drawn
  expect_equal(wide_label_gap[[1]]$rule, "single")
  expect_equal(wide_body_gap[[1]]$rule, "single")
})

test_that("Every header row is spaced, not just the outside of the block", {
  # A spanned header has to keep the space between its levels. Padding only the
  # top and bottom of the block renders it shorter than the reference, which is
  # what the CDISC pilot hit on its multi row header tables (#97)
  dat <- data.frame(pid = "01", sid = "701", a = "1", b = "2")

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_column_headers(
      pid = c("", "Pooled", "Id"),
      sid = c("", "Site", "Id"),
      a = c("Treatment", "ITT", "n"),
      b = c("Treatment", "Eff", "n")
    )

  rows <- docx_spacing(clin_header_pad(ct, above = 18, below = 4))

  # All three header rows, not only the first and last
  for (i in 1:3) {
    expect_equal(rows[[i]]$top, 18, label = paste("header row", i, "top"))
    expect_equal(rows[[i]]$bottom, 4, label = paste("header row", i, "bottom"))
  }

  # Which is exactly what padding the header part by hand produces - this is
  # the call the pilot had to keep using
  by_hand <- function(x, ...) {
    x <- clinify_table_default(x)
    flextable::padding(x, padding.top = 18, padding.bottom = 4, part = "header")
  }
  reference <- withr::with_options(
    list(clinify_table_default = by_hand),
    docx_spacing(ct)
  )

  expect_equal(
    lapply(rows[1:3], `[`, c("top", "bottom")),
    lapply(reference[1:3], `[`, c("top", "bottom"))
  )
})

test_that("The gap under the rule reaches the first row of every page", {
  dat <- data.frame(pg = rep(1:2, each = 2), v1 = as.character(1:4))

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_page_by("pg") |>
    clin_header_pad(rule_to_body = 12)

  for (page in 1:2) {
    rows <- docx_spacing(ct, table = page)

    # First body row of the page carries the gap, the rest do not
    expect_equal(rows[[2]]$top, 12, label = paste("page", page, "first row"))
    expect_equal(rows[[3]]$top, 0.1, label = paste("page", page, "second row"))
  }
})

test_that("A group label leaves the buffer with the column labels", {
  dat <- data.frame(
    grp = rep(c("A", "B"), each = 2),
    v1 = as.character(1:4)
  )

  ct <- clintable(dat, use_labels = FALSE) |>
    clin_group_by("grp") |>
    clin_header_pad(above = 18)

  ct <- prep_pagination_(finish_table_(getOption("clinify_table_default")(ct)))
  tbl <- get_table_(ct, ct$clinify_config$pagination_idx[[1]])

  # The label is added above the header, so the space asked for above the
  # column labels stays with them rather than moving to the top of the block
  top_padding <- tbl$header$styles$pars$padding.top$data[, 1]
  expect_equal(unname(top_padding[2]), 18)
})

test_that("The HTML preview survives configured header spacing", {
  ct <- clintable(head(mtcars[, 1:2], 3)) |>
    clin_header_pad(above = 18, below = 4, rule_to_body = 6)
  expect_no_error(clintable_as_html(ct))

  dat <- data.frame(pg = rep(1:2, each = 2), v1 = as.character(1:4))
  paged <- clintable(dat, use_labels = FALSE) |>
    clin_page_by("pg") |>
    clin_header_pad(rule_to_body = 12)
  expect_no_error(clintable_as_html(paged))
})

test_that("Header spacing can differ per row", {
  # A house wide buffer has to be able to sit alongside the handful of tables
  # that need a different gap on one row (#113)
  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_column_headers(mpg = c("Drug A", "n"), cyl = c("Drug B", "n"))

  top <- function(ct) {
    unname(finish_table_(ct)$header$styles$pars$padding.top$data[, 1])
  }

  # One value per row
  expect_equal(top(clin_header_pad(ct, above = c(18, 34))), c(18, 34))

  # Or aim the call at one row and leave the other alone
  expect_equal(top(clin_header_pad(ct, above = 18, rows = 1))[1], 18)
  expect_equal(top(clin_header_pad(ct, above = 18, rows = 2))[2], 18)

  # A single value still covers every row
  expect_equal(top(clin_header_pad(ct, above = 18)), c(18, 18))
})

test_that("Spacing aimed at some rows leaves a per row exception alone", {
  # The verb is applied as the table renders, after anything the caller did, so
  # a call covering every row overwrites a padding() set beforehand - `rows` is
  # how the exception is kept (#113)
  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_column_headers(mpg = c("Drug A", "n"), cyl = c("Drug B", "n")) |>
    flextable::padding(i = 2, padding.top = 34, part = "header")

  top <- function(ct) {
    unname(finish_table_(ct)$header$styles$pars$padding.top$data[, 1])
  }

  # Untouched, the exception is there
  expect_equal(top(ct)[2], 34)

  # Covering every row replaces it, which is what the call asked for
  expect_equal(top(clin_header_pad(ct, above = 18)), c(18, 18))

  # Aiming at row 1 keeps it
  expect_equal(top(clin_header_pad(ct, above = 18, rows = 1)), c(18, 34))
})

test_that("Per row header spacing is validated", {
  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_column_headers(mpg = c("Drug A", "n"), cyl = c("Drug B", "n"))

  # A value per row has to match the rows it is aimed at
  expect_error(
    finish_table_(clin_header_pad(ct, above = c(18, 34, 99))),
    "one for each of the 2 rows"
  )
  expect_error(
    finish_table_(clin_header_pad(ct, above = c(18, 34), rows = 1)),
    "one for each of the 1 rows"
  )

  # Rows have to be usable row numbers that the header actually has
  expect_error(clin_header_pad(ct, above = 18, rows = 0), "1 or more")
  expect_error(clin_header_pad(ct, above = 18, rows = 1.5), "whole header row")
  expect_error(
    finish_table_(clin_header_pad(ct, above = 18, rows = c(1, 9))),
    "between 1 and 2"
  )

  # There is only one first body row to space away from the rule
  expect_error(
    clin_header_pad(ct, rule_to_body = c(1, 2)),
    "single number of points"
  )
})

test_that("A second call refines the first rather than replacing it", {
  # Same silent loss as #119, in the other verb that carries several settings
  base <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_column_headers(mpg = c("Sp", "x"), cyl = c("Sp", "y"))

  refined <- base |>
    clin_header_pad(above = 18, below = 4) |>
    clin_header_pad(rule_to_body = 6)

  pad <- refined$clinify_config$header_pad
  expect_equal(pad$above, 18)
  expect_equal(pad$below, 4)
  expect_equal(pad$rule_to_body, 6)

  # A value named again is replaced
  expect_equal(
    clin_header_pad(clin_header_pad(base, above = 18, below = 4), below = 9)$clinify_config$header_pad$below,
    9
  )
})
