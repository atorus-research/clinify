# Set the spacing around a table's column headers

Three pieces of vertical space shape the header block, and they are
named here for where they sit rather than for the padding that produces
them, because the mapping between the two is not obvious:

## Usage

``` r
clin_header_pad(x, above = NULL, below = NULL, rule_to_body = NULL)
```

## Arguments

- x:

  A clintable object

- above:

  Space above each header row, in points

- below:

  Space below each header row, in points. The bottom row's is what sets
  how far the rule sits from the column labels

- rule_to_body:

  Space between that rule and the first body row, in points

## Value

A clintable object

## Details

- `above` is the space over each header row. On a single row header that
  is the buffer above the column labels; on a spanned header it also
  opens the space between the levels, which is what a blank row above
  the header normally looks like.

- `below` is the space under each header row. The one that matters most
  is the bottom row's, because a cell's bottom border sits at the bottom
  edge of the cell, *below* its padding - so this is what decides how
  far the rule is drawn from the column labels. It does not open space
  beneath the rule.

- `rule_to_body` is the space between that rule and the first row of the
  table body, which is the one that has to come from the body side.

`above` and `below` apply to every row of the header, which is the usual
convention and matches `flextable::padding(part = "header")`. To space a
single header row differently, reach for
[`flextable::padding()`](https://davidgohel.github.io/flextable/reference/padding.html)
with an `i` directly.

`rule_to_body` is applied to the first row of every page, so a table
split over pages keeps the same gap under the rule throughout. If a
group label is added above the header it keeps its own spacing, since it
is put there as the table renders.

Spacing is given in points, which is what flextable measures cell
padding in. Whatever is set here replaces the header padding clinify
starts with.

## Examples

``` r
# A blank row's worth of space around each header row, the rule close under
# the labels, and a little air before the body starts
clintable(mtcars) |>
  clin_header_pad(above = 18, below = 4, rule_to_body = 6)


.cl-f16c7ac6{}.cl-f1637c8c{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f1671f22{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:4pt;padding-top:18pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f1671f36{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:6pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f1671f37{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f16748e4{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f16748ee{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```
