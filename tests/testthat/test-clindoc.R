# The sequence of top level body elements: "tbl" for a table, "break" for a
# paragraph holding a page break, anything else dropped
body_sequence <- function(doc) {
  body <- xml2::xml_find_first(
    officer::docx_body_xml(doc),
    "/w:document/w:body"
  )
  kids <- xml2::xml_children(body)
  out <- vapply(
    kids,
    \(k) {
      if (xml2::xml_name(k) == "tbl") {
        "tbl"
      } else if (
        length(xml2::xml_find_all(k, ".//w:br[@w:type='page']")) > 0
      ) {
        "break"
      } else {
        NA_character_
      }
    },
    character(1)
  )
  out[!is.na(out)]
}

# Two pages: rows split by `page`
paged_table <- function() {
  dat <- head(mtcars[1:4], 10)
  dat$page <- rep(1:2, each = 5)
  clintable(dat) |>
    clin_page_by("page")
}

default_table <- function() {
  clintable(head(iris, 5))
}

test_that("Paginated tables are written page by page with breaks between", {
  doc <- clindoc(paged_table(), paged_table())

  expect_equal(
    body_sequence(doc),
    c("tbl", "break", "tbl", "break", "tbl", "break", "tbl")
  )
})

test_that("Mixed default and paginated tables keep their order", {
  # Paginated last: no trailing break after its final page
  doc <- clindoc(default_table(), paged_table())
  expect_equal(
    body_sequence(doc),
    c("tbl", "break", "tbl", "break", "tbl")
  )

  # Default last, after buffered pages that have to be flushed first
  doc <- clindoc(paged_table(), default_table())
  expect_equal(
    body_sequence(doc),
    c("tbl", "break", "tbl", "break", "tbl")
  )

  # Default tables only never open an append context
  doc <- clindoc(default_table(), default_table(), default_table())
  expect_equal(
    body_sequence(doc),
    c("tbl", "break", "tbl", "break", "tbl")
  )
})

test_that("Paginated content is written through a single append context", {
  # Each context serializes and re-parses the whole document, so opening one
  # per table made building a document quadratic in the number of tables
  opened <- 0
  start_context <- officer::body_append_start_context
  local_mocked_bindings(
    body_append_start_context = function(...) {
      opened <<- opened + 1
      start_context(...)
    },
    .package = "officer"
  )

  doc <- clindoc(rep(list(paged_table()), 5))

  expect_equal(opened, 1)
  expect_equal(sum(body_sequence(doc) == "tbl"), 10)
  expect_equal(sum(body_sequence(doc) == "break"), 9)
})

test_that("A single clintable in a list goes through as_clindoc()", {
  ct <- paged_table() |>
    clin_add_titles(list("A title"))

  doc <- clindoc(list(ct))

  expect_s3_class(doc, "clindoc")
  expect_equal(doc$clinify_config$titles, ct$clinify_config$titles)
  expect_equal(body_sequence(doc), c("tbl", "break", "tbl"))
})

test_that("clindoc() rejects objects that aren't clintables", {
  expect_error(clindoc(default_table(), flextable::flextable(head(iris))))
})
