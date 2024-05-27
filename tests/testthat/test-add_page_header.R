# `add_page_*` family of functions shares similar approach on creating flextable objects that later can
# be formatted accordingly, so the testing approach is almost identical.

test_that("The output of the function is a flextable object.", {
    t1 <- add_page_header(list(c("your title goes here")))

    expect_s3_class(t1, 'flextable')
})

test_that("The number of elements passed to the function call matches the number of rows in the flextable.", {
    t1 <- add_page_header(list(
        "centered text",
        "another text in the center"
    ))
    t2 <- add_page_header(list(c("left-aligned text", "right-aligned text")))
    t3 <- add_page_header(list(
        c("left-aligned text", "some text in the center", "right-aligned text"),
        c("I want this to be on the left", "this is definitely a right thing"),
        c("just a cool title")
    ))

    expect_equal(nrow(t1$body$dataset), 2)
    expect_equal(nrow(t2$body$dataset), 1)
    expect_equal(nrow(t3$body$dataset), 3)
})

test_that("When one element is passed into the sublist, we expect it to be centered.", {
    t1 <- add_page_header(list(
        "this text should appear in each of the three table's columns to become merged and centered in the docx file."
    ))
    values <- as.character(unlist(t1$body$dataset))
    expect_equal(values[1], values[2])
    expect_equal(values[1], values[3])
})

test_that("When two elements are passed into the sublist, we expect the first to be left-aligned and the last - right-aligned.", {
    t1 <- add_page_header(list(c(
        "this text should only appear in the 1st column so it can be left-aligned in the docx file.",
        "this text should appear in the 2nd and the 3rd columns so they are merged and the resulted text is right aligned in the docx file."
    )))
    values <- as.character(unlist(t1$body$dataset))
    expect_true(values[1] != values[2])
    expect_equal(values[2], values[3])
})

test_that("When three elements are passed into the sublist, we expect the first to be left-aligned, the last - right-aligned, and the middle one to be centered.", {
    t1 <- add_page_header(list(c(
        "this text should only appear in the 1st column so it can be left-aligned in the docx file.",
        "text only for the 2nd columnd.",
        "this will appear only in 3rd column."
    )))
    values <- as.character(unlist(t1$body$dataset))
    expect_true(values[1] != values[2])
    expect_true(values[1] != values[3])
    expect_true(values[2] != values[3])
})
