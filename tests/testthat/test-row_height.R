basic_table <- function() {
  clintable(head(mtcars[, 1:3], 4)) |>
    clin_add_titles(list(c("Protocol", "Page {PAGE}"), "Table 1")) |>
    clin_add_footnotes(list("A footnote"))
}

# Row heights as Word records them, in points, per part of the document
docx_pitch <- function(ct) {
  file <- withr::local_tempfile(fileext = ".docx")
  write_clindoc(ct, file = file)

  unzipped <- withr::local_tempdir()
  utils::unzip(file, exdir = unzipped)

  part_pitch <- function(part) {
    path <- file.path(unzipped, "word", part)
    if (!file.exists(path)) {
      return(character(0))
    }

    xml <- paste(readLines(path, warn = FALSE), collapse = "")
    found <- regmatches(
      xml,
      gregexpr('<w:trHeight w:val="[0-9]+" w:hRule="[a-zA-Z]+"/>', xml)
    )[[1]]

    unique(paste0(
      as.numeric(sub('.*val="([0-9]+)".*', "\\1", found)) / 20,
      "pt/",
      sub('.*hRule="([a-zA-Z]+)".*', "\\1", found)
    ))
  }

  list(
    body = part_pitch("document.xml"),
    titles = part_pitch("header1.xml"),
    footnotes = part_pitch("footer1.xml")
  )
}

test_that("clin_row_height records what it was given", {
  ct <- basic_table()

  expect_null(ct$clinify_config$row_height)

  cfg <- clin_row_height(
    ct,
    body = 15.35,
    title = 11.4,
    footnote = 11.4
  )$clinify_config$row_height

  # Held in inches, the unit flextable measures in
  expect_equal(cfg$body, 15.35 / 72)
  expect_equal(cfg$title, 11.4 / 72)
  expect_equal(cfg$footnote, 11.4 / 72)
  expect_equal(cfg$rule, "atleast")

  # Only what was asked for
  partial <- clin_row_height(ct, body = 15.35)$clinify_config$row_height
  expect_equal(partial$body, 15.35 / 72)
  expect_null(partial$title)
  expect_null(partial$footnote)
})

test_that("Row height can be given in any of the supported units", {
  height <- function(...) {
    clin_row_height(basic_table(), ...)$clinify_config$row_height$body
  }

  expect_equal(height(body = 18), 0.25)
  expect_equal(height(body = 0.25, unit = "in"), 0.25)
  expect_equal(height(body = 2.54, unit = "cm"), 1)
  expect_equal(height(body = 25.4, unit = "mm"), 1)
})

test_that("clin_row_height is validated", {
  ct <- basic_table()

  expect_error(clin_row_height(ct), "At least one of body, title, or footnote")
  expect_error(clin_row_height(ct, body = "x"), "single positive number")
  expect_error(clin_row_height(ct, body = 0), "single positive number")
  expect_error(clin_row_height(ct, body = -1), "single positive number")
  expect_error(clin_row_height(ct, body = c(1, 2)), "single positive number")
  expect_error(clin_row_height(ct, body = NA), "single positive number")
  expect_error(clin_row_height(ct, title = "x"), "`title` must be")

  expect_error(
    clin_row_height(ct, body = 1, rule = "tight"),
    "should be one of"
  )
  expect_error(
    clin_row_height(ct, body = 1, unit = "twips"),
    "should be one of"
  )
  expect_error(clin_row_height(mtcars, body = 1), "inherits")
})

test_that("Row height reaches every surface of the Word document", {
  # Stock tables are left where flextable puts them, which is a nominal quarter
  # inch and a rule that lets the renderer decide
  stock <- docx_pitch(basic_table())
  expect_equal(stock$titles, "18pt/auto")
  expect_equal(stock$footnotes, "18pt/auto")

  # The pitch the CDISC pilot ships
  pitched <- docx_pitch(
    clin_row_height(basic_table(), body = 15.35, title = 11.4, footnote = 11.4)
  )
  expect_equal(pitched$titles, "11.4pt/atLeast")
  expect_equal(pitched$footnotes, "11.4pt/atLeast")
  expect_true("15.35pt/atLeast" %in% pitched$body)

  # Titles and footnotes are reachable independently of the body
  body_only <- docx_pitch(clin_row_height(basic_table(), body = 15.35))
  expect_equal(body_only$titles, "18pt/auto")
  expect_true("15.35pt/atLeast" %in% body_only$body)
})

test_that("The rule is passed through", {
  for (rule in c("atleast", "exact", "auto")) {
    pitched <- docx_pitch(
      clin_row_height(basic_table(), body = 15.35, rule = rule)
    )
    expect_true(
      any(grepl(tolower(rule), tolower(pitched$body), fixed = TRUE)),
      label = paste("rule", rule)
    )
  }
})

test_that("A footnote page takes the footnote pitch", {
  ct <- basic_table() |>
    clin_add_footnote_page(list("Lots of footnotes")) |>
    clin_row_height(footnote = 11.4)

  # The footnote page is a table in the body of the document
  expect_true("11.4pt/atLeast" %in% docx_pitch(ct)$body)
})

test_that("A table's own pitch beats a house style", {
  # An organisation's styling function sets the house pitch, and a table that
  # asks for its own has to win - the styling function runs first
  house <- function(x, ...) {
    x <- clinify_table_default(x)
    x <- flextable::hrule(x, rule = "atleast")
    flextable::height_all(x, height = 20 / 72)
  }
  withr::local_options(clinify_table_default = house)

  expect_true("20pt/atLeast" %in% docx_pitch(basic_table())$body)
  expect_true(
    "15.35pt/atLeast" %in%
      docx_pitch(clin_row_height(basic_table(), body = 15.35))$body
  )
})

test_that("Rows added while rendering take the body pitch", {
  dat <- data.frame(
    grp = rep(c("A", "B"), each = 3),
    cap = rep(c("Caption A", "Caption B"), each = 3),
    v1 = as.character(1:6),
    v2 = as.character(7:12)
  )

  page_one <- function(ct) {
    ct <- finish_table_(getOption("clinify_table_default")(ct))
    ct <- prep_pagination_(ct)
    get_table_(ct, ct$clinify_config$pagination_idx[[1]])
  }

  grouped <- function(...) {
    ct <- clintable(dat, use_labels = FALSE) |>
      clin_group_by("grp", caption_by = "cap")
    if (...length()) clin_row_height(ct, ...) else ct
  }

  # Nothing configured leaves both where they were
  plain <- page_one(grouped())
  expect_equal(plain$header$rowheights * 72, c(18, 18))
  expect_equal(plain$footer$rowheights * 72, 18)

  pitched <- page_one(grouped(body = 15.35))

  # The group label is the top header row and takes the body pitch, while the
  # column header row is left content driven
  expect_equal(pitched$header$rowheights * 72, c(15.35, 18))
  expect_equal(pitched$header$hrule, c("atleast", "auto"))

  # And so does the caption
  expect_equal(pitched$footer$rowheights * 72, 15.35)
  expect_equal(pitched$footer$hrule, "atleast")
})

test_that("Pagination keeps the table's configuration", {
  # Dropping the group and page columns used to take the whole config with
  # them, because the slice did not carry it forward
  ct <- clintable(
    data.frame(
      grp = rep(c("A", "B"), each = 2),
      v1 = as.character(1:4)
    ),
    use_labels = FALSE
  ) |>
    clin_group_by("grp") |>
    clin_row_height(body = 15.35) |>
    clin_table_align("left")

  prepped <- prep_pagination_(ct)

  expect_equal(prepped$clinify_config$row_height$body, 15.35 / 72)
  expect_equal(prepped$clinify_config$table_align, "left")
  expect_equal(prepped$clinify_config$group_by, "grp")
  expect_false(is.null(prepped$clinify_config$pagination_idx))
})

test_that("A clindoc carries the pitch of the clintable it came from", {
  ct <- clin_row_height(basic_table(), body = 15.35, title = 11.4)
  doc <- as_clindoc(ct)

  expect_equal(doc$clinify_config$row_height$body, 15.35 / 72)
  expect_equal(doc$clinify_config$row_height$title, 11.4 / 72)

  # And the titles it renders are on that pitch
  expect_equal(docx_pitch(doc)$titles, "11.4pt/atLeast")
})

test_that("The HTML preview survives a configured pitch", {
  ct <- clin_row_height(basic_table(), body = 15.35, title = 11.4)
  expect_no_error(clintable_as_html(ct))

  dat <- data.frame(
    grp = rep(c("A", "B"), each = 3),
    v1 = as.character(1:6)
  )
  grouped <- clintable(dat, use_labels = FALSE) |>
    clin_group_by("grp") |>
    clin_row_height(body = 15.35)
  expect_no_error(clintable_as_html(grouped))
})
