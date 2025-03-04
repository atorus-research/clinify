test_that("Errors checking", {

  # Proper target object
  expect_error(clin_column_headers(1), "inherits")

  # Proper argument types
  expect_error(clin_column_headers(clintable(iris), drat=1), "All header arguments")


  # Proper column names
  expect_error(clin_column_headers(clintable(iris), blah="blah"), "All argument names")
})

test_that("Headers apply as expected", {
  ct <- clintable(iris)

  ct2 <- ct |> 
    clin_column_headers(
      Sepal.Length = c("Flowers", "Sepal", "Length"),  
      Sepal.Width = c("Flowers", "Sepal", "Width"),  
      Petal.Length = c("Petal", "Length"),  
      Petal.Width = c("Petal", "Width")
    )

  # These snapshots capture the major factors of interest
  expect_snapshot(ct2$header$dataset) # Dup values appied in right spots
  expect_snapshot(ct2$header$spans) # Blank column spans don't merge with horizontals

  # Use iris
  refdat <- iris
  attr(refdat$Sepal.Length, 'label') <- "Flower||Sepal||Length"
  attr(refdat$Sepal.Width, 'label') <- "Flower||Sepal||Width"
  attr(refdat$Petal.Length, 'label') <- "Flower||Petal||Length"
  attr(refdat$Petal.Width, 'label') <- "Flower||Petal||Width"

  ct <- clintable(refdat)
  expect_snapshot(ct2$header$dataset) # Dup values appied in right spots
  expect_snapshot(ct2$header$spans) # Blank column spans don't merge with horizontals

})