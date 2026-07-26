test_that("Error messages", {
  ct <- clintable(mtcars)

  expect_error(
    clin_add_titles(ct, ls = list("x"), ft = new_title_footnote(list("x"))),
    "One of"
  )

  expect_error(
    clin_add_titles(ct, ls = list(c("1", "2", "3"))),
    "All sublists must"
  )
})


test_that("Titles and footnotes can be attached", {
  ct <- clintable(mtcars) %>%
    # Add titles here is using new_header_footer to allow flextable functions
    # to customzie the titles block
    clin_add_titles(
      ft = new_title_footnote(
        list(
          c("left aligned", "right aligned"),
          c("Single element")
        ),
        "titles"
      ) %>%
        border_remove()
    ) %>%
    # Adding footnotes is just using a list of lists instead to show how it can
    # be automatically converted
    clin_add_footnotes(
      list(
        c("left aligned", "right aligned"),
        c("", "Single element")
      )
    )

  expect_true(all(c("titles", "footnotes") %in% names(ct$clinify_config)))
  out <- clintable_as_html(ct)

  # Need to improve this but for now, make sure that the output contains 3
  # tables - one for the header, one for the footer, and one for the table body
  html_out <- xml2::read_html(out[[3]])
  expect_equal(
    length(xml2::xml_find_all(html_out, "body//*/table")),
    3
  )
})

test_that("align places each title or footnote line", {
  ls <- list(
    c("Protocol: ABC", "Page {PAGE}"),
    "Table 14-2.01",
    "Summary of Demographics"
  )

  line_align <- function(ft) {
    unname(ft$body$styles$pars$text.align$data[, 1])
  }

  # Titles centre a lone line, footnotes send it left, and a pair is split
  expect_equal(
    line_align(new_title_footnote(ls, "titles")),
    c("left", "center", "center")
  )
  expect_equal(
    line_align(new_title_footnote(ls, "footnotes")),
    c("left", "left", "left")
  )

  # A single line can be placed anywhere, which is what #98 asked for
  expect_equal(
    line_align(new_title_footnote(ls, "titles", c(NA, NA, "left"))),
    c("left", "center", "left")
  )
  expect_equal(
    line_align(new_title_footnote(ls, "titles", c("split", "right", "left"))),
    c("left", "right", "left")
  )

  # One value covers every line
  expect_equal(
    line_align(new_title_footnote(list("a", "b"), "titles", "right")),
    c("right", "right")
  )

  # The line still takes the full width, so placing it is all that changed
  ft <- new_title_footnote(list("Just the one"), "titles", "left")
  expect_equal(ft$body$spans$rows[1, ], c(2, 0))
  expect_equal(unname(unlist(ft$body$dataset[1, ])), rep("Just the one", 2))
})

test_that("The duplicate text trick still left aligns a single line", {
  # This was the only way to do it before align existed, and code in the wild
  # depends on it
  ft <- new_title_footnote(list(c("Left me", "Left me")), "titles")
  expect_equal(unname(ft$body$styles$pars$text.align$data[, 1]), "left")
  expect_equal(ft$body$spans$rows[1, ], c(2, 0))
})

test_that("align is validated", {
  ls <- list(c("a", "b"), "c")

  expect_error(new_title_footnote(ls, "titles", "nope"), "must be one of")
  expect_error(new_title_footnote(ls, "titles", c("left", "left", "left")), "length 1 or 2")
  expect_error(new_title_footnote(ls, "titles", 1), "character vector")

  # A split line cannot be placed, and a lone line cannot be split
  expect_error(
    new_title_footnote(ls, "titles", c("left", "left")),
    "always split down the middle"
  )
  expect_error(
    new_title_footnote(ls, "titles", c("split", "split")),
    "cannot be split"
  )

  # Lines have to hold something
  expect_error(
    new_title_footnote(list(character(0)), "titles"),
    "needs at least one element"
  )

  # A prebuilt flextable is already aligned however the user aligned it
  expect_error(
    clin_add_titles(clintable(mtcars), ft = new_title_footnote(ls, "titles"), align = "left"),
    "cannot be used with ft"
  )
})

test_that("align reaches the verbs", {
  ct <- clintable(mtcars) |>
    clin_add_titles(list("Centered by default", "Sent left"), align = c(NA, "left")) |>
    clin_add_footnotes(list("Right for once"), align = "right")

  expect_equal(
    unname(ct$clinify_config$titles$body$styles$pars$text.align$data[, 1]),
    c("center", "left")
  )
  expect_equal(
    unname(ct$clinify_config$footnotes$body$styles$pars$text.align$data[, 1]),
    "right"
  )
})

spec_for_tests <- function() {
  data.frame(
    type = c("title", "title", "footnote", "footnote", "footnote_page"),
    text1 = c(
      "Protocol: CDISCPILOT01",
      "Table 14-2.01",
      "Source: {FILE}",
      "Generated {DATE}",
      "Extra notes"
    ),
    text2 = c("Page {PAGE} of {NUMPAGES}", NA, NA, "Page {PAGE}", NA),
    align = c("split", "center", "left", "split", "left"),
    stringsAsFactors = FALSE
  )
}

line_text <- function(ft) {
  apply(ft$body$dataset, 1, \(row) paste(unique(row), collapse = " | "))
}

test_that("One spec feeds titles, footnotes and a footnote page", {
  spec <- spec_for_tests()

  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_add_titles(spec) |>
    clin_add_footnotes(spec) |>
    clin_add_footnote_page(spec)

  # Each function takes only its own rows, in the order given
  expect_equal(
    unname(line_text(ct$clinify_config$titles)),
    c("Protocol: CDISCPILOT01 | Page {PAGE} of {NUMPAGES}", "Table 14-2.01")
  )
  expect_equal(
    unname(line_text(ct$clinify_config$footnotes)),
    c("Source: {FILE}", "Generated {DATE} | Page {PAGE}")
  )
  expect_equal(
    unname(line_text(ct$clinify_config$footnote_page)),
    "Extra notes"
  )

  # And the align column places them
  expect_equal(
    unname(ct$clinify_config$titles$body$styles$pars$text.align$data[, 1]),
    c("left", "center")
  )
})

test_that("A surface with no rows in the spec is left alone", {
  # This is what lets one spec be handed to all three functions
  spec <- spec_for_tests()
  spec <- spec[spec$type != "footnote_page", ]

  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_add_titles(spec) |>
    clin_add_footnote_page(spec)

  expect_false(is.null(ct$clinify_config$titles))
  expect_null(ct$clinify_config$footnote_page)
})

test_that("A spec needs only type and text1", {
  spec <- data.frame(type = c("title", "footnote"), text1 = c("A title", "A note"))

  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_add_titles(spec) |>
    clin_add_footnotes(spec)

  expect_equal(unname(line_text(ct$clinify_config$titles)), "A title")
  expect_equal(unname(line_text(ct$clinify_config$footnotes)), "A note")

  # Without an align column the lines take their default placement, which
  # centers a lone title and sends a lone footnote left
  expect_equal(
    unname(ct$clinify_config$titles$body$styles$pars$text.align$data[, 1]),
    "center"
  )
  expect_equal(
    unname(ct$clinify_config$footnotes$body$styles$pars$text.align$data[, 1]),
    "left"
  )
})

test_that("Plural type values are accepted", {
  spec <- data.frame(type = c("titles", "footnotes"), text1 = c("T", "F"))

  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_add_titles(spec) |>
    clin_add_footnotes(spec)

  expect_equal(unname(line_text(ct$clinify_config$titles)), "T")
  expect_equal(unname(line_text(ct$clinify_config$footnotes)), "F")
})

test_that("Tokens fill in placeholders without touching page numbers", {
  spec <- spec_for_tests()

  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_add_footnotes(
      spec,
      tokens = list(FILE = "programs/t-14-2.01.R", DATE = "2026-07-25")
    )

  text <- unname(line_text(ct$clinify_config$footnotes))
  expect_equal(text[1], "Source: programs/t-14-2.01.R")

  # {PAGE} is deliberately left for clin_replace_pagenums() to turn into a
  # real Word field
  expect_true(grepl("{PAGE}", text[2], fixed = TRUE))
  expect_true(grepl("2026-07-25", text[2], fixed = TRUE))
})

test_that("Tokens work on a plain list too", {
  ct <- clintable(head(mtcars[, 1:2], 2)) |>
    clin_add_titles(
      list("Source: {FILE}", c("Left {A}", "Right {A}")),
      tokens = c(FILE = "x.R", A = "filled")
    )

  expect_equal(
    unname(line_text(ct$clinify_config$titles)),
    c("Source: x.R", "Left filled | Right filled")
  )
})

test_that("Spec and token problems are reported clearly", {
  ct <- clintable(head(mtcars[, 1:2], 2))
  spec <- spec_for_tests()

  expect_error(
    clin_add_titles(ct, data.frame(text1 = "a")),
    "needs a `type` column",
    fixed = TRUE
  )
  expect_error(
    clin_add_titles(ct, data.frame(type = "title")),
    "needs a `text1` column",
    fixed = TRUE
  )
  expect_error(
    clin_add_titles(ct, data.frame(type = "ttile", text1 = "a")),
    "Not recognized"
  )

  # Two ways of saying the same thing at once
  expect_error(clin_add_titles(ct, spec, align = "left"), "drop one of them")

  # A prebuilt flextable is already written and aligned
  built <- new_title_footnote(list("a"), "titles")
  expect_error(
    clin_add_titles(ct, ft = built, tokens = list(A = "b")),
    "cannot be used with ft"
  )

  expect_error(
    clin_add_titles(ct, list("a"), tokens = list("b")),
    "named list or character vector"
  )
  expect_error(
    clin_add_titles(ct, list("a"), tokens = list(A = c("b", "c"))),
    "single, non missing value"
  )
})
