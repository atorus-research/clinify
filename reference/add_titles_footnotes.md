# Add titles, footnotes, or a footnote page to a clintable or clindoc

This function allows you to attach specified titles, footnotes, or a
footnote page into clintable or clindoc object. The input can be
provided either as a list of character vectors, or pre-built flextable.

## Usage

``` r
clin_add_titles(x, ls = NULL, ft = NULL)

clin_add_footnotes(x, ls = NULL, ft = NULL)

clin_add_footnote_page(x, ls = NULL, ft = NULL)
```

## Arguments

- x:

  a clintable object

- ls:

  a list of character vectors, no more than 2 elements to a vector

- ft:

  A flextable object to use as the header

## Value

A clintable object

## Details

When using the `ls` parameter, each element of the list can contain no
more than two elements within each character vector. In a title, a
single element will align center. In a footnote, a single element will
align to the left. For both titles and footnotes, two elements will
align split down the middle, with the left side element aligning left
and the right side element aligning right. In a title, a single left
aligned element, provide a 2 element character vector with duplicate
values.

## Examples

``` r
clintable(mtcars) |>
  clin_add_titles(
    list(
      c("Left", "Right"),
      c("Just the middle")
    )
  ) |>
  clin_add_footnotes(
    list(
      c(
        "Here's a footnote.",
        format(Sys.time(), "%H:%M %A, %B %d, %Y")
      )
    )
  ) |>
  clin_add_footnote_page(
    list(
      c(
        "Use when you have a lot of footnotes",
        "And you don't want to put them on every page"
      )
    )
  )

  

.cl-b9ee4f9a{}.cl-b9e74862{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-b9ea3c0c{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-b9ea3c20{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-b9ea3c2a{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-b9ea672c{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-b9ea6736{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-b9ea6740{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Left
```
