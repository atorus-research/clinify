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
