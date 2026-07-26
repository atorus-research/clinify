# Set the row height of a clintable

Regulatory outputs are usually specified to an exact row pitch, and the
rendered height of a row is what decides how much fits on a page.
flextable leaves rows at a nominal quarter inch with a rule of "auto",
which lets the renderer size them however it likes. This records the
pitch you want and applies it when the table renders.

## Usage

``` r
clin_row_height(
  x,
  body = NULL,
  title = NULL,
  footnote = NULL,
  rule = c("atleast", "exact", "auto"),
  unit = c("pt", "in", "cm", "mm")
)
```

## Arguments

- x:

  A clintable object

- body:

  Row pitch for the table body

- title:

  Row pitch for the title lines

- footnote:

  Row pitch for the footnote lines, and for a footnote page

- rule:

  How the renderer should treat the pitch. `"atleast"` (the default)
  makes it a floor, so a cell whose text wraps grows past it instead of
  being clipped. `"exact"` pins the row to the pitch and clips anything
  that does not fit, which is the only way to get a pitch at or below
  the font's line height. `"auto"` lets the renderer decide, ignoring
  the pitch.

- unit:

  Unit the pitches are given in. Row pitch is normally specified in
  points, so that is the default.

## Value

A clintable object

## Details

The three surfaces are set separately because they are separate tables:
the table body, and the title and footnote blocks that go into the Word
header and footer. Group label and caption rows, which clinify inserts
while it renders, take the body pitch.

The height is applied to whole parts, so it is a pitch for every row of
the surface rather than a per-row height. Anything already set with
[`flextable::height()`](https://davidgohel.github.io/flextable/reference/height.html)
or
[`flextable::height_all()`](https://davidgohel.github.io/flextable/reference/height.html)
is replaced. Because the pitch is applied after the default styling
functions run, it also holds when an organisation's
[`clinify_table_default()`](https://atorus-research.github.io/clinify/reference/clinify_defaults.md)
sets a house pitch of its own - the table's own setting wins.

## Examples

``` r
clintable(mtcars) |>
  clin_row_height(body = 15.35, title = 11.4, footnote = 11.4)


.cl-f2be340a{}.cl-f2b343ba{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f2b6a4d8{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:9pt;padding-top:9pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f2b6a4ec{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0.1pt;padding-top:0.1pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f2b6cd8c{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f2b6cda0{width:0.75in;height:0.213in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


mpg
```
