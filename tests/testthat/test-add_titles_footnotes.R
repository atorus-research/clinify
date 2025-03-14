test_that("Error messages", {
  ct <- clintable(mtcars)

  expect_error(
    clin_add_titles(ct, ls = list("x"), ft = new_title_footnote(list("x"))),
    "One of"
  )

  expect_error(
    clin_add_titles(ct, ls = list(c("1", "2", "3", "4"))),
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
          c("left aligned", "centered", "right aligned"),
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
        c("left aligned", "centered", "right aligned"),
        c("left aligned", "right aligned"),
        c("", "", "Single element")
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
