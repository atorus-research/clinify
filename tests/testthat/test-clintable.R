test_that("Creation functions", {
  ct1 <- clintable(mtcars)
  ct2 <- clintable(mtcars, group_by = "gear")

  expect_equal(ct1$clinify_config$pagination_method, "default")
  expect_equal(ct2$clinify_config$pagination_method, "custom")
})
