# Set how a table sits across the page

flextable centres a table on the page. Regulatory outputs are usually
flush left, and a narrow table sometimes wants to be centred
deliberately. The choice is recorded on the clintable and applied when
the table renders, after the default styling function has run, so it
holds even when an organisation's own
[`clinify_table_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
rebuilds the table properties.

## Usage

``` r
clin_table_align(x, align)
```

## Arguments

- x:

  A clintable object

- align:

  One of "left", "center", or "right"

## Value

A clintable object

## Details

This is only about where the table sits across the page. How wide it is,
and how that width is divided between the columns, is
[`clin_col_widths()`](https://atorus-research.github.io/clinify/reference/clin_col_widths.md).

## Examples

``` r
clintable(mtcars) |>
  clin_table_align("left")


.cl-f3765616{}.cl-f36e0326{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f37168c2{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f37168d6{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f3718f32{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f3718f3c{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```
