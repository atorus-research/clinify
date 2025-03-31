test_that("build_word_fields correctly splits a simple string", {
  result <- build_word_fields("Page {PAGE} of {NUMPAGES}")

  expect_equal(length(result), 4)
  expect_equal(result[[1]], "Page ")
  expect_equal(result[[2]], bquote(as_word_field(x = "PAGE")))
  expect_equal(result[[3]], " of ")
  expect_equal(result[[4]], bquote(as_word_field(x = "NUMPAGES")))
})

test_that("build_word_fields handles a string with only one placeholder", {
  result <- build_word_fields("Total {NUMPAGES} pages")

  expect_equal(length(result), 3)
  expect_equal(result[[1]], "Total ")
  expect_equal(result[[2]], bquote(as_word_field(x = "NUMPAGES")))
  expect_equal(result[[3]], " pages")
})

test_that("build_word_fields works when placeholders are at the start or end", {
  result <- build_word_fields("{PAGE} of {NUMPAGES}")

  expect_equal(length(result), 3)
  expect_equal(result[[1]], bquote(as_word_field(x = "PAGE")))
  expect_equal(result[[2]], " of ")
  expect_equal(result[[3]], bquote(as_word_field(x = "NUMPAGES")))

  result2 <- build_word_fields("Page {PAGE}")

  expect_equal(length(result2), 2)
  expect_equal(result2[[1]], "Page ")
  expect_equal(result2[[2]], bquote(as_word_field(x = "PAGE")))
})

test_that("build_word_fields returns the input when there are no placeholders", {
  result <- build_word_fields("No placeholders here")

  expect_equal(length(result), 1)
  expect_equal(result[[1]], "No placeholders here")
})

test_that("build_word_fields handles consecutive placeholders correctly", {
  result <- build_word_fields("{PAGE}{NUMPAGES}")

  expect_equal(length(result), 2)
  expect_equal(result[[1]], bquote(as_word_field(x = "PAGE")))
  expect_equal(result[[2]], bquote(as_word_field(x = "NUMPAGES")))
})

test_that("build_word_fields handles an empty string", {
  result <- build_word_fields("")

  expect_equal(length(result), 1)
  expect_equal(result[[1]], "")
})

test_that("clin_replace_pagenums processes fields properly", {
  title <- new_title_footnote(
    list(
      # We'll add tools to automate paging
      c("Protocol: CDISCPILOT01", "Page {PAGE} of {NUMPAGES}"),
      c("Table 14-2.01"),
      c("Summary of Demographic and Baseline Characteristics")
    ),
    "titles"
  )

  title <- clin_replace_pagenums(title)

  expect_snapshot(title$body$content$data[1, "Center"])
  expect_snapshot(title$body$content$data[1, "Right"])

  footnote <- new_title_footnote(
    list(
      # We'll add tools to automate paging
      c("Page {PAGE}", "Total Pages: {NUMPAGES}")
    ),
    "footnotes"
  )

  footnote <- clin_replace_pagenums(footnote)

  expect_snapshot(footnote$body$content$data[1, "Left"])
  expect_snapshot(footnote$body$content$data[1, "Center"])
  expect_snapshot(footnote$body$content$data[1, "Right"])
})
