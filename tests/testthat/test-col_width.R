test_that("col_width argument checks", {
  expect_error(clin_col_widths(1), "inherits")
  expect_error(
    clin_col_widths(clintable(mtcars), drat = 'a'),
    "All width arguments"
  )
  expect_error(
    clin_col_widths(clintable(mtcars), blah = .2),
    "The following columns"
  )
  expect_error(
    clin_col_widths(clintable(mtcars), drat = 2),
    "Width arguments"
  )
})