test_that("padding works", {
  ct1 <- clintable(mtcars) |>
    clin_group_pad('gear', when = 'change')

  pad_data <- ct1$body$styles$pars$padding.top$data
  expect_snapshot(pad_data)

  mtcars2 <- mtcars
  # Mock blank repeats
  mtcars2[!find_split_inds(mtcars2$gear, 'change'), 'gear'] <- ''

  ct2 <- clintable(mtcars2) |>
    clin_group_pad('gear', when = 'notempty')

  pad_data2 <- ct2$body$styles$pars$padding.top$data

  expect_equal(pad_data2, pad_data)
})
