# Create a new clintable object

A clintable object directly inherits from a flextable object. This
function will pass all necessary parameters
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
and conver the object to a `clintable`

## Usage

``` r
clintable(x, page_by = NULL, group_by = NULL, use_labels = TRUE, ...)
```

## Arguments

- x:

  A data frame

- page_by:

  A variable in the input dataframe to use for pagination

- group_by:

  A character vector of variable names which will be used for grouping
  and attached as a label above the table headers

- use_labels:

  Use variable labels as column headers. Nested levels can be achieved
  using the string "\|\|" as a delimitter. Horizontal and vertical
  levels using identical words will be merged.

- ...:

  Parameters to pass to
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)

## Value

A clintable object

## Examples

``` r
clintable(mtcars)


.cl-bea6e998{}.cl-be9e1cfa{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-bea1b6da{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bea1b6ee{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bea1dfa2{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bea1dfac{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```
