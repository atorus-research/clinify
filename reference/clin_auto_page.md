# Enable Word Auto-Pagination Using Group Variable

This function uses the applies the functionality
[`flextable::keep_with_next()`](https://davidgohel.github.io/flextable/reference/keep_with_next.html)
by automatically building the row indices using some grouping variable.
Each group identified by the variable (i.e. when the value of the
variable changes) will be set as a "keep_with_next" group in Word. Using
this functionality, Word will attempt not to break that group across
pages, enabling smoother pagination without having to do specific
calculations of page breaks.

## Usage

``` r
clin_auto_page(x, group_var, when = c("change", "notempty"), drop = FALSE)
```

## Arguments

- x:

  A clintable object

- group_var:

  A string containing a variable name of the input dataset used to
  calculate groups

- when:

  Character string indicating when to apply padding:

  - `"notempty"`: Find allowable break points when the value in
    `group_var` is not empty.

  - `"change"`: Find allowable break points when the value in
    `group_var` changes from the previous row.

- drop:

  Keep or drop the \`group_var“ variable

## Value

A clintable object

## Examples

``` r

clintable(mtcars) |>
  clin_auto_page("gear")


.cl-bb29f7ec{}.cl-bb215632{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-bb24e752{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bb24e766{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bb24e770{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bb250d2c{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bb250d36{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```
