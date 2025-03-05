test_that("Single page standard print", {
  x <- knit_print(clintable(mtcars))

  tab_html <- rvest::read_html(as.character(x)) |> 
    rvest::html_elements(".tabwid")

  expect_equal(length(tab_html), 1)

  extracted_data <- as.data.frame(rvest::html_table(tab_html))
  comp <- mtcars[1:15,]
  rownames(comp) <- NULL
  expect_equal(extracted_data, comp)
})


test_that("Alternating pages print 3 pages", {
  dat <- mtcars
  dat['page'] <- c(
    rep(1, 10),
    rep(2, 10),
    rep(3, 10),
    c(4, 4)
  )
  dat2 <- rbind(dat, dat)
  dat2['groups1'] <- c(
    rep('a', 32),
    rep('b', 32)
  )
  dat2['groups2'] <- c(
    rep('1', 16),
    rep('2', 16),
    rep('1', 16),
    rep('2', 16)
  )
  
  # Create a basic table
  ct <- clintable(dat2) |> 
    clin_page_by('page') |> 
    clin_group_by(c('groups1', 'groups2')) |> 
    clin_alt_pages(
      key_cols = c('mpg', 'cyl', 'hp'),
      col_groups = list(
        c('disp', 'drat', 'wt'),
        c('qsec', 'vs', 'am'),
        c('gear', 'carb')
      ) 
    ) |> 
    clin_col_widths(mpg = .2, cyl=.2, disp=.15, vs=.15) |>
    clin_add_titles(
      list(
        c("Left", "Center", "Right"),
        c("Just the middle")
      )
    ) |> 
    clin_add_footnotes(
        list(
          c(
            "Here's a footnote.", 
            "11:58 Wednesday, March 05, 2025"
          )
        )
      )
  
  x <- knit_print.clintable(ct)
  
  tab_html <- rvest::read_html(as.character(x)) |> 
    rvest::html_elements(".page") 

  # 3 pages of alternating pages
  expect_snapshot(as.data.frame(rvest::html_table(tab_html[[1]])))
  expect_snapshot(as.data.frame(rvest::html_table(tab_html[[2]])))
  expect_snapshot(as.data.frame(rvest::html_table(tab_html[[3]])))
})




