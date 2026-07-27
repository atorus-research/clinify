test_that("Creation functions", {
  ct1 <- clintable(mtcars)
  ct2 <- clintable(mtcars, group_by = "gear")

  expect_equal(ct1$clinify_config$pagination_method, "default")
  expect_equal(ct2$clinify_config$pagination_method, "custom")
})

# The text flextable baked into the body cells, which is what actually reaches
# Word. Reading it back is the only way to see the numeric reformatting that
# `coerce_character` exists to avoid.
body_text <- function(ct) {
  chunks <- flextable::information_data_chunk(ct)
  chunks <- chunks[chunks$.part == "body", ]
  split(chunks$txt, factor(chunks$.col_id, levels = ct$col_keys))
}

# Every verbatim rendering test needs a table built both ways off the same data
coerced_text <- function(dat) {
  body_text(clintable(dat, use_labels = FALSE, coerce_character = TRUE))
}

as_built_text <- function(dat) {
  body_text(clintable(dat, use_labels = FALSE))
}

test_that("coerce_character renders pre-formatted values verbatim", {
  # A treatment column holding a count in one row and a mean in another has to
  # be a double, and flextable formats a double column as a whole. That makes
  # the count 86 render against its neighbour as "86.0" (#104)
  dat <- data.frame(
    row_label = c("n", "Mean"),
    trt_a = c(86, 75.2)
  )

  expect_equal(as_built_text(dat)$trt_a, c("86.0", "75.2"))
  expect_equal(coerced_text(dat)$trt_a, c("86", "75.2"))
})

test_that("coerce_character suppresses big.mark and the seven digit rounding", {
  # The two other ways a column wide format() decision rewrites an already
  # formatted value: big.mark reaches a whole number that needed no separator,
  # and the default seven significant digits silently rounds a long value
  dat <- data.frame(
    counts = c(1234, 12.5),
    stats = c(1234567.891, 2)
  )

  as_built <- as_built_text(dat)
  expect_equal(as_built$counts, c("1,234.0", "12.5"))
  expect_equal(as_built$stats, c("1,234,568", "2"))

  coerced <- coerced_text(dat)
  expect_equal(coerced$counts, c("1234", "12.5"))
  expect_equal(coerced$stats, c("1234567.891", "2"))
})

test_that("coerce_character leaves NA rendering as a blank cell", {
  # `as.character(NA)` is NA_character_ rather than "", and flextable's default
  # `na_str` is "", so the blank cell survives coercion without clinify having
  # to substitute "" itself
  dat <- data.frame(
    num = c(NA, 0.0421),
    chr = c("x", NA_character_)
  )

  coerced <- coerced_text(dat)
  expect_equal(coerced$num, c("", "0.0421"))
  expect_equal(coerced$chr, c("x", ""))

  # And the underlying data really is NA, not "" - the distinction matters to
  # the pagination variables tested below
  ct <- clintable(dat, use_labels = FALSE, coerce_character = TRUE)
  expect_identical(ct$body$dataset$chr, c("x", NA_character_))
})

test_that("coerce_character keeps labels so use_labels still builds headers", {
  # The obvious `x[] <- lapply(x, as.character)` drops every attribute,
  # including `label`, which would silently collapse label driven headers to
  # bare column names
  dat <- data.frame(
    row_label = "Age (years)",
    trt_a = 86,
    trt_b = 75.2
  )
  attr(dat$row_label, "label") <- "||Parameter"
  attr(dat$trt_a, "label") <- "Treatment||A"
  attr(dat$trt_b, "label") <- "Treatment||B"

  ct <- clintable(dat, coerce_character = TRUE)

  # Two header rows, with the spanner merged across the treatment columns,
  # exactly as building without coercion produces
  expect_equal(flextable::nrow_part(ct, part = "header"), 2)
  expect_equal(
    unlist(ct$header$dataset[2, ]),
    c(row_label = "Parameter", trt_a = "A", trt_b = "B")
  )
  expect_equal(ct$header$spans$rows[1, ], c(1, 2, 0))

  # The labels themselves are still on the data, and the columns are character
  expect_equal(
    attr(ct$body$dataset$trt_a, "label", exact = TRUE),
    "Treatment||A"
  )
  expect_true(all(vapply(ct$body$dataset, is.character, logical(1))))
})

test_that("coerce_character does not mistake haven value labels for a label", {
  # `attr(x, "label")` partial matches haven's `labels` attribute, which would
  # put a whole named vector of value labels where the header text belongs
  dat <- data.frame(sex = c(1, 2))
  attr(dat$sex, "labels") <- c(Male = 1, Female = 2)

  ct <- clintable(dat, coerce_character = TRUE)

  expect_null(attr(ct$body$dataset$sex, "label", exact = TRUE))
  expect_equal(flextable::nrow_part(ct, part = "header"), 1)
  expect_equal(ct$header$dataset$sex, "sex")
})

test_that("coerce_character takes factor levels rather than integer codes", {
  # A factor is an integer vector underneath, so the codes are what would show
  # up if the coercion went through the storage type
  dat <- data.frame(
    grade = factor(
      c("Grade 3", "Grade 1"),
      levels = c("Grade 1", "Grade 2", "Grade 3")
    )
  )

  expect_equal(coerced_text(dat)$grade, c("Grade 3", "Grade 1"))
})

test_that("coerce_character defaults to FALSE and changes nothing when off", {
  # This is the backwards compatibility guarantee: not asking for coercion has
  # to produce precisely the object the previous release produced
  ct <- clintable(mtcars, group_by = "gear")

  expect_identical(
    clintable(mtcars, group_by = "gear", coerce_character = FALSE),
    ct
  )
  expect_false(identical(
    clintable(mtcars, group_by = "gear", coerce_character = TRUE),
    ct
  ))
})

test_that("coerce_character rejects anything but a single TRUE or FALSE", {
  # A silently ignored typo here changes the numbers in the output, so it has
  # to be caught rather than treated as truthy
  bad <- "must be either TRUE or FALSE"
  expect_error(clintable(mtcars, coerce_character = "TRUE"), bad)
  expect_error(clintable(mtcars, coerce_character = NA), bad)
  expect_error(clintable(mtcars, coerce_character = c(TRUE, TRUE)), bad)
})

test_that("coerce_character carries the verbatim text through to Word", {
  # Word is the first class output, so the round trip has to be checked in
  # word/document.xml and not just in the flextable object
  dat <- data.frame(
    row_label = c("n", "Mean"),
    trt_a = c(86, 75.2)
  )

  document_xml <- function(ct) {
    file <- withr::local_tempfile(fileext = ".docx")
    write_clindoc(ct, file = file)

    unzipped <- withr::local_tempdir()
    utils::unzip(file, files = "word/document.xml", exdir = unzipped)
    paste(
      readLines(file.path(unzipped, "word", "document.xml"), warn = FALSE),
      collapse = ""
    )
  }

  coerced <- document_xml(
    clintable(dat, use_labels = FALSE, coerce_character = TRUE)
  )
  expect_match(coerced, ">86<", fixed = TRUE)
  expect_no_match(coerced, ">86.0<", fixed = TRUE)

  as_built <- document_xml(clintable(dat, use_labels = FALSE))
  expect_match(as_built, ">86.0<", fixed = TRUE)
})

test_that("NA is left as NA, which collapses a page variable padded with it", {
  # Coercion deliberately stops short of replacing NA with "". The two are
  # interchangeable in a body column, but `clin_page_by()` splits on
  # `x != lag(x)`, which is NA wherever either side is NA, and `which()` drops
  # those rows instead of splitting on them. Documented on `clintable()` so the
  # `x[is.na(x)] <- ""` half of the old boilerplate is not dropped blindly
  pages <- function(ct) {
    length(prep_pagination_(ct)$clinify_config$pagination_idx)
  }

  by_page <- function(page_var) {
    dat <- data.frame(pg = page_var, v = c("a", "b", "c", "d"))
    pages(clin_page_by(
      clintable(dat, use_labels = FALSE, coerce_character = TRUE),
      "pg"
    ))
  }

  # A fully populated page variable is unaffected
  expect_equal(by_page(c("A", "A", "B", "B")), 2)

  # Padding it with "" keeps every value distinct from the row above, so every
  # row starts a page - which is what the pilot boilerplate was producing
  expect_equal(by_page(c("A", "", "B", "")), 4)

  # Padding with NA loses the split entirely and yields one page
  expect_equal(by_page(c("A", NA, "B", NA)), 1)

  # `when = "notempty"` tests against "" instead, and handles NA correctly
  na_padded <- data.frame(pg = c("A", NA, "B", NA), v = c("a", "b", "c", "d"))
  by_group <- clin_group_by(
    clintable(na_padded, use_labels = FALSE, coerce_character = TRUE),
    "pg",
    when = "notempty"
  )
  expect_equal(pages(by_group), 2)
})

test_that("as_clintable has no coerce_character, but has a substitute", {
  # A flextable arrives with its text already rendered, so there is no source
  # data left to coerce. `as_clintable()` documents
  # `flextable::set_formatter()` instead, which rewrites the body from the
  # stored data - the promise that route makes is checked here
  expect_false("coerce_character" %in% names(formals(as_clintable)))

  dat <- data.frame(
    row_label = c("n", "Mean"),
    trt_a = c(86, 75.2)
  )
  ft <- flextable::flextable(dat)

  expect_equal(body_text(as_clintable(ft))$trt_a, c("86.0", "75.2"))
  expect_equal(
    body_text(
      as_clintable(flextable::set_formatter(ft, values = as.character))
    )$trt_a,
    c("86", "75.2")
  )
})

test_that("Value labels are not mistaken for a variable label", {
  # haven attaches a `labels` attribute of value labels to coded variables, and
  # attr() partial matches, so a request for `label` was being answered by
  # `labels` and read as header text (#107)
  d <- data.frame(a = 1:2, b = 3:4)
  attr(d$a, "labels") <- c(Low = 1, High = 2)

  expect_false(has_labels_(d))

  # A real variable label is still found
  d2 <- data.frame(v = 1)
  attr(d2$v, "label") <- "Treatment A||(N=86)"
  expect_true(has_labels_(d2))
  expect_equal(nrow(clintable(d2)$header$dataset), 2)

  # And when a column carries both, the real label is the one used
  d3 <- data.frame(v = 1)
  attr(d3$v, "labels") <- c(A = 1)
  attr(d3$v, "label") <- "Real||Label"
  expect_equal(
    unname(unlist(clintable(d3)$header$dataset)),
    c("Real", "Label")
  )
})

test_that("use_labels = FALSE really leaves labels alone", {
  # flextable reads column labels of its own accord, so without being told not
  # to it put the raw label string, delimiter and all, into the header even
  # when the caller had asked for labels to be left alone
  d <- data.frame(lbl = "Male", val = 86)
  attr(d$val, "label") <- "Treatment A||(N=86)"

  rendered <- function(ct) {
    flextable:::information_data_chunk(ct)$txt
  }

  expect_false(any(grepl("Treatment A", rendered(clintable(d, use_labels = FALSE)))))
  expect_true(any(grepl("Treatment A", rendered(clintable(d)))))

  # Which also gives a way past the flextable side of #107
  coded <- data.frame(a = 1:2)
  attr(coded$a, "labels") <- c(Low = 1, High = 2)
  expect_no_error(clintable(coded, use_labels = FALSE))
})

test_that("Value labels still reach the cells when labels are wanted", {
  # Turning labels off must not be the only way to render a table, so the
  # flextable behaviour clinify relies on has to stay intact
  d <- data.frame(v = c(1, 2))
  attr(d$v, "label") <- "Sex"
  attr(d$v, "labels") <- c(Male = 1, Female = 2)

  txt <- flextable:::information_data_chunk(clintable(d))$txt
  expect_true(all(c("Male", "Female") %in% txt))
})

test_that("Setting column headers keeps the spacing clinify starts them with", {
  # Setting the headers rebuilds the header part, which dropped the padding put
  # on it at construction and left a custom-header table on flextable's own
  # default while a plain one kept clinify's (#101)
  pad <- function(ct) {
    list(
      top = unname(ct$header$styles$pars$padding.top$data[, 1]),
      bottom = unname(ct$header$styles$pars$padding.bottom$data[, 1])
    )
  }

  plain <- pad(clintable(mtcars))
  expect_equal(plain$top, 9)
  expect_equal(plain$bottom, 9)

  # A two level header carries it on the outside of the block, top of the first
  # row and bottom of the last
  custom <- pad(clin_column_headers(clintable(mtcars), mpg = c("A", "B")))
  expect_equal(custom$top, c(9, 5))
  expect_equal(custom$bottom, c(5, 9))

  # Same when the header came from column labels, which is the default path
  labelled <- mtcars
  attr(labelled$mpg, "label") <- "Miles||per gallon"
  expect_equal(pad(clintable(labelled))$top, c(9, 5))

  # It is a starting point, so anything the caller does afterwards still wins
  by_hand <- flextable::padding(
    clin_column_headers(clintable(mtcars), mpg = c("A", "B")),
    i = 1,
    part = "header",
    padding.top = 21
  )
  expect_equal(pad(by_hand)$top, c(21, 5))

  configured <- finish_table_(
    clin_header_pad(
      clin_column_headers(clintable(mtcars), mpg = c("A", "B")),
      above = 18,
      below = 4
    )
  )
  expect_equal(unique(pad(configured)$top), 18)
})
