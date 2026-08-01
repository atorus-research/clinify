# Configure alternating pages during pagination of a clintable

This function configures alternating pages on a clintable object.

## Usage

``` r
clin_alt_pages(x, key_cols, col_groups)
```

## Arguments

- x:

  A clintable object

- key_cols:

  A character vector of variable names

- col_groups:

  A list of character vectors of variable names

## Value

A clintable object

## Examples

``` r
ct <- clintable(mtcars)

clin_alt_pages(
  ct,
  key_cols = c("mpg", "cyl", "hp"),
  col_groups = list(
    c("disp", "drat", "wt"),
    c("qsec", "vs", "am"),
    c("gear", "carb")
  )
)
#> NOTE: Alternating pages were set, but no selection for row wise pagination was configured Defaulting to 20 rows per page.

  

.cl-94cd19ee{}.cl-94c3385c{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-94c67080{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-94c67094{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-94c69876{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-94c69880{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```
