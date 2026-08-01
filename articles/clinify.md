# Getting Started

The primary motivation behind **{clinify}** is to take the things that
are great about the R packages {flextable} and {officer}, take the
standard and complex pieces of formatting clinical tables for regulatory
use, and simplify the tedious pieces. {flextable} and {officer} offer a
huge range of capability for creating tables in R and rendering them to
various formats. {flextable} makes formatting the table itself
straightforward, while {officer} gives you lower level access to create
documents like docx files and insert separate components as needed.

When working with clinical tables, the devil is in the details. Every
organization tends to have its own bits of nuance, and the flexibility
of each organization to deviate from those standards varies. In the R
world, there have still been a number of features that have either made
generating clinical tables with certain features very tedious, or in
some cases not possible with the current tooling. With **{clinify}** we
attempt to close that gap and give some quality-of-life features to
programmers making these tables.

### Basics

Let’s start at the beginning

``` r

library(clinify)
library(flextable)
library(officer)

ct <- clintable(mtcars)
print(ct)
```

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.620 | 16.46 | 0   | 1   | 4    | 4    |
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.875 | 17.02 | 0   | 1   | 4    | 4    |
| 22.8 | 4   | 108.0 | 93  | 3.85 | 2.320 | 18.61 | 1   | 1   | 4    | 1    |
| 21.4 | 6   | 258.0 | 110 | 3.08 | 3.215 | 19.44 | 1   | 0   | 3    | 1    |
| 18.7 | 8   | 360.0 | 175 | 3.15 | 3.440 | 17.02 | 0   | 0   | 3    | 2    |
| 18.1 | 6   | 225.0 | 105 | 2.76 | 3.460 | 20.22 | 1   | 0   | 3    | 1    |
| 14.3 | 8   | 360.0 | 245 | 3.21 | 3.570 | 15.84 | 0   | 0   | 3    | 4    |
| 24.4 | 4   | 146.7 | 62  | 3.69 | 3.190 | 20.00 | 1   | 0   | 4    | 2    |
| 22.8 | 4   | 140.8 | 95  | 3.92 | 3.150 | 22.90 | 1   | 0   | 4    | 2    |
| 19.2 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.30 | 1   | 0   | 4    | 4    |
| 17.8 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.90 | 1   | 0   | 4    | 4    |
| 16.4 | 8   | 275.8 | 180 | 3.07 | 4.070 | 17.40 | 0   | 0   | 3    | 3    |
| 17.3 | 8   | 275.8 | 180 | 3.07 | 3.730 | 17.60 | 0   | 0   | 3    | 3    |
| 15.2 | 8   | 275.8 | 180 | 3.07 | 3.780 | 18.00 | 0   | 0   | 3    | 3    |
| 10.4 | 8   | 472.0 | 205 | 2.93 | 5.250 | 17.98 | 0   | 0   | 3    | 4    |

In **{clinify}**, a clintable itself is at it’s root a `flextable`
object with some extra metadata attached to it. A core part of the
design philosophy of **{clinify}** is to build off of {flextable} at its
core, extending functionality so that flextable functions are still
operable on a `clintable` object.

The table printed above is the foundation of a `clintable` object. What
we see here is the print method of a `clintable` being used. Compared to
flextable, the primary thing that has is the application of default
styling. Organizations generally have specific style preferences for
their outputs, such as font and font size, standard conventions for
borders, page size and margins, etc. These are configuration in
**{clinify}** using some standard options that will be explained in
another vignette. The [`print()`](https://rdrr.io/r/base/print.html)
method respects these settings to allow you to interactively explore
your table being formatted.

### Titles and Footnotes

Let’s expand some features of the table.

``` r

ct <- clintable(mtcars) |>
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
  )

print(ct)
```

|                 |       |
|-----------------|-------|
| Left            | Right |
| Just the middle |       |

| mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb |
|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.620 | 16.46 | 0   | 1   | 4    | 4    |
| 21.0 | 6   | 160.0 | 110 | 3.90 | 2.875 | 17.02 | 0   | 1   | 4    | 4    |
| 22.8 | 4   | 108.0 | 93  | 3.85 | 2.320 | 18.61 | 1   | 1   | 4    | 1    |
| 21.4 | 6   | 258.0 | 110 | 3.08 | 3.215 | 19.44 | 1   | 0   | 3    | 1    |
| 18.7 | 8   | 360.0 | 175 | 3.15 | 3.440 | 17.02 | 0   | 0   | 3    | 2    |
| 18.1 | 6   | 225.0 | 105 | 2.76 | 3.460 | 20.22 | 1   | 0   | 3    | 1    |
| 14.3 | 8   | 360.0 | 245 | 3.21 | 3.570 | 15.84 | 0   | 0   | 3    | 4    |
| 24.4 | 4   | 146.7 | 62  | 3.69 | 3.190 | 20.00 | 1   | 0   | 4    | 2    |
| 22.8 | 4   | 140.8 | 95  | 3.92 | 3.150 | 22.90 | 1   | 0   | 4    | 2    |
| 19.2 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.30 | 1   | 0   | 4    | 4    |
| 17.8 | 6   | 167.6 | 123 | 3.92 | 3.440 | 18.90 | 1   | 0   | 4    | 4    |
| 16.4 | 8   | 275.8 | 180 | 3.07 | 4.070 | 17.40 | 0   | 0   | 3    | 3    |
| 17.3 | 8   | 275.8 | 180 | 3.07 | 3.730 | 17.60 | 0   | 0   | 3    | 3    |
| 15.2 | 8   | 275.8 | 180 | 3.07 | 3.780 | 18.00 | 0   | 0   | 3    | 3    |
| 10.4 | 8   | 472.0 | 205 | 2.93 | 5.250 | 17.98 | 0   | 0   | 3    | 4    |

|                    |                                 |
|--------------------|---------------------------------|
| Here's a footnote. | 19:59 Saturday, August 01, 2026 |

Here we’ve added some titles and footnotes to the document. The
functions
[`clin_add_titles()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
and
[`clin_add_footnotes()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
allow you to insert titles and footnotes into the `clintable` metadata.
When the `clintable` is written to a document or printed, the titles are
respected. When you’re printing interactively, the HTML that’s rendered
allows you to see the titles and footnotes above and below the table as
if you’re viewing an individual page. When writing to a docx file, the
titles are placed in the header and the footnotes are placed into the
footer.

In this example, by providing a list of character vectors, each element
of the list is added as a new line. There are some broad assumptions
being made:

- In titles, a single element will align center. In footnotes, the
  element will align right.
- If two elements are provided, they align left and right.
- If three elements are provided, they align left, right, and center.

Ultimately, the attached tables are converted to flextables. As such,
you can create your own flextable and attach it to the header or footer
using the `ft` option in
[`clin_add_titles()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md).
We also have the helper function
[`new_title_footnote()`](https://atorus-research.github.io/clinify/reference/new_title_footnote.md)
that allows you to supply a list and generate the flextable so you can
apply extra formatting as desired.

### Pagination and Alternating Pages

Let’s look at a couple more functions.

``` r

dat <- mtcars
dat["page"] <- c(
  rep(1, 10),
  rep(2, 10),
  rep(3, 10),
  c(4, 4)
)
dat2 <- rbind(dat, dat)
dat2["groups1"] <- c(
  rep("a", 32),
  rep("b", 32)
)
dat2["groups2"] <- c(
  rep("1", 16),
  rep("2", 16),
  rep("1", 16),
  rep("2", 16)
)

dat2["captions"] <- c(
  rep("Caption 1", 16),
  rep("Caption 2", 16),
  rep("Caption 3", 16),
  rep("Caption 4", 16)
)

# Create a basic table
ct <- clintable(dat2) |>
  clin_page_by("page") |>
  clin_group_by(c("groups1", "groups2")) |>
  clin_alt_pages(
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  ) |>
  clin_col_widths(mpg = .2, cyl = .2, disp = .15, vs = .15) |>
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
  )

print(ct)
```

|                 |       |
|-----------------|-------|
| Left            | Right |
| Just the middle |       |

[TABLE]

|                    |                                 |
|--------------------|---------------------------------|
| Here's a footnote. | 19:59 Saturday, August 01, 2026 |

|                 |       |
|-----------------|-------|
| Left            | Right |
| Just the middle |       |

[TABLE]

|                    |                                 |
|--------------------|---------------------------------|
| Here's a footnote. | 19:59 Saturday, August 01, 2026 |

|                 |       |
|-----------------|-------|
| Left            | Right |
| Just the middle |       |

[TABLE]

|                    |                                 |
|--------------------|---------------------------------|
| Here's a footnote. | 19:59 Saturday, August 01, 2026 |

1

2

3

A number of new things have happened here. Let’s go through function by
function.

First, we’ve used the function
[`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md)
manually specify how page breaks should be handled. This is a data
driven function, so here we’ve specified to use the `page` variable from
the `dat2` dataframe. Each time this variable **changes**, a page break
will be inserted. Note that this isn’t used for sorting, just inserting
a change break.

Next, we’ve used the function
[`clin_group_by()`](https://atorus-research.github.io/clinify/reference/clin_group_by.md).
This allows us to put by lines *above* the column headers using data
from the input data frame. Similar to
[`clin_page_by()`](https://atorus-research.github.io/clinify/reference/clin_page_by.md)
each time this value changes, a new page will start. Just like the table
data, these lines above the column headers will always reflect the data
from within the variable. You can use as many group variable as you need
here.

Next we’ve used
[`clin_alt_pages()`](https://atorus-research.github.io/clinify/reference/clin_alt_pages.md).
This has been one of the most requested features we’ve seen since we
originally developed the package {pharmaRTF}. This feature is designed
to handle cases where the number of variables you need to present
overflow the width of the page that you have available. The function
works with rotating pages, so the same data rows are presented for each
overflowing page, necessary, while the columns being presented change.
After the first input of the `clintable` object, you have two
parameters:

- `key_cols`: Columns that should be fixed to each page being presented
- `col_groups`: The groups of columns that should be presented on each
  of the alternating pages

If we look at that function specifically:

``` r

ct |>
  clin_alt_pages(
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  )
```

In total there will be 3 alternating pages. The columns presented on
each page will be:

- `mpg`, `cyl`, `hp`, `disp`, `drat`, `wt`
- `mpg`, `cyl`, `hp`, `qsec`, `vs`, `am`
- `mpg`, `cyl`, `hp`, `gear`, and `carb`

To make things easier for the developer, while developing interactively
the print method has been updated to print 3 pages as styled HTML into
the viewer pane, where you can select the page of choice from a page
selector below the table. When printing to a word document, pages are
inserted in the proper order. The logic works as follows:

- Identify chunk of rows for a given page. This can be based on the
  `page_by` method to manually insert page breaks or you can select
  maximum rows to print to a single page
- Identify the separate columns necessary for the individual pages
- Write out the chunk of rows for each set of `col_groups` in order
  before jumping to the next set of rows.

## Column Headers and Widths

From the previous example, another **{clinify}** function we used was
`clin_col_width()`. The goal of this function is simply to make setting
your column widths for a table straightforward. By default, in
{flextable} your column widths are based on a unit such as inches or
centimeters. In `clin_col_width()` we allow you to use the proportion of
the page that you’d like that column to fill. From the syntax above:

``` r

ct |>
  clin_col_widths(mpg = .2, cyl = .2, disp = .15, vs = .15)
```

In this case, we’re saying that:

- `mpg` will fill 20% of the page
- `cyl` will fill 20% of the page
- `disp` will fill 15% of the page
- `vs` will fill 15% of the page

Widths deal with the horizontal;
[`clin_row_height()`](https://atorus-research.github.io/clinify/reference/clin_row_height.md)
deals with the vertical. Regulatory outputs are usually specified to an
exact row pitch, and how tall a row renders is what decides how much
fits on a page. {flextable} leaves rows at a nominal quarter inch with a
rule of “auto”, which lets the renderer size them however it likes, so a
table can drift down the page and paginate differently than the
specification says it should. Pitches are given in points, since that is
how they are normally specified:

``` r

clintable(dat2) |>
  clin_row_height(body = 15.35, title = 11.4, footnote = 11.4)
```

The body, the titles and the footnotes are set separately because they
are separate tables. Group label and caption rows take the body pitch.
The default `rule = "atleast"` treats the pitch as a floor, so a cell
whose text wraps grows past it rather than being clipped;
`rule = "exact"` pins every row to the pitch, which is the only way to
get a pitch tighter than the font’s line height, at the cost of clipping
anything that does not fit.

Three pieces of vertical space shape the header block, and
[`clin_header_pad()`](https://atorus-research.github.io/clinify/reference/clin_header_pad.md)
names them for where they sit rather than for the padding that produces
them:

``` r

clintable(dat2) |>
  clin_header_pad(above = 18, below = 4, rule_to_body = 6)
```

`above` and `below` are the room over and under each header row - on a
single row header that is the buffer above the column labels and the
distance down to the rule, and on a spanned header it also opens the
space between the levels. `rule_to_body` is the room between that rule
and the first body row. `below` and `rule_to_body` are not
interchangeable: a cell’s bottom border is drawn at the bottom edge of
the cell, below its padding, so padding under the header pushes the rule
away from the labels and toward the body rather than opening space
beneath it. Space under the rule has to come from the body side, which
is what `rule_to_body` does - on the first row of every page, so a table
split over pages keeps the same gap throughout.

Where a table sits across the page is
[`clin_table_align()`](https://atorus-research.github.io/clinify/reference/clin_table_align.md).
{flextable} centres tables, and regulatory outputs are usually flush
left.

The rest of the columns will be spaced evenly based on the remaining
space. The space that’s filled is based on default configurations for
page width, which are configurable within your session. Furthermore,
`clin_col_width()` adapts to alternating pages. The ratios given to key
columns apply for each alternating page, and the proportions applied to
the additional `col_group` variables adapt any remaining space to ensure
a page fits the total page width.

One last tedious part of structuring any table is getting the table
headers formatted correctly. There are a couple specific features, such
as spanning headers, which can also be tricky to get right, especially
in a semi-automated way. For this reason, we’ve added the function
[`clin_column_headers()`](https://atorus-research.github.io/clinify/reference/clin_column_headers.md)
to make this process a bit easier. Let’s use `iris` as an example of a
table to which we want to apply some spanning headers.

``` r

clintable(iris) |>
  clin_column_headers(
    Sepal.Length = c("Flowers", "Sepal", "Length"),
    Sepal.Width = c("Flowers", "Sepal", "Width"),
    Petal.Length = c("Petal", "Length"),
    Petal.Width = c("Petal", "Width")
  )
```

| Flowers |       |        |       |        |
|---------|-------|--------|-------|--------|
| Sepal   |       | Petal  |       |        |
| Length  | Width | Length | Width |        |
| 5.1     | 3.5   | 1.4    | 0.2   | setosa |
| 4.9     | 3.0   | 1.4    | 0.2   | setosa |
| 4.7     | 3.2   | 1.3    | 0.2   | setosa |
| 4.6     | 3.1   | 1.5    | 0.2   | setosa |
| 5.0     | 3.6   | 1.4    | 0.2   | setosa |
| 5.4     | 3.9   | 1.7    | 0.4   | setosa |
| 4.6     | 3.4   | 1.4    | 0.3   | setosa |
| 5.0     | 3.4   | 1.5    | 0.2   | setosa |
| 4.4     | 2.9   | 1.4    | 0.2   | setosa |
| 4.9     | 3.1   | 1.5    | 0.1   | setosa |
| 5.4     | 3.7   | 1.5    | 0.2   | setosa |
| 4.8     | 3.4   | 1.6    | 0.2   | setosa |
| 4.8     | 3.0   | 1.4    | 0.1   | setosa |
| 4.3     | 3.0   | 1.1    | 0.1   | setosa |
| 5.8     | 4.0   | 1.2    | 0.2   | setosa |

The first parameter of
[`clin_column_headers()`](https://atorus-research.github.io/clinify/reference/clin_column_headers.md)
will be the `clintable` object for which you want headers to apply. From
there, use the column name to which you’re applying a header. The way
this function works is to use a character element for row of headers you
want to apply. So for example, if you need three rows of headers, you
can use 3 elements for a single column. The elements go to their
respective rows.

When using spanning headers, you’re also typically using cell merging so
that a single string of text spans over multiple columns. To accomplish
this, repeat the text that you want to merge and ensure that those
elements are placed in the same row. Consider the example above. For the
Sepal variable, we have two spanning headers. One for “flowers”, and one
for “Sepal”. These cells will be merged, and the bottom row contains
“Length” and “Width” separately.

Sometimes a header row repeats a label across adjacent columns without
those columns being one spanner - a shift table where several columns
each sit under their own treatment arm but share a “Baseline” sub-label,
for example. Merging those cells would be wrong, so the `merge` argument
lets you say which header rows should have their identical cells merged.
`merge = "spanners"` covers the common case by leaving the bottom row of
the header - the one holding the individual column labels - alone.

``` r

clintable(iris) |>
  clin_column_headers(
    Sepal.Length = c("Flowers", "Sepal", "Value"),
    Sepal.Width = c("Flowers", "Sepal", "Value"),
    Petal.Length = c("Petal", "Value"),
    Petal.Width = c("Petal", "Value"),
    merge = "spanners"
  )
```

| Flowers |       |       |       |        |
|---------|-------|-------|-------|--------|
| Sepal   |       | Petal |       |        |
| Value   | Value | Value | Value |        |
| 5.1     | 3.5   | 1.4   | 0.2   | setosa |
| 4.9     | 3.0   | 1.4   | 0.2   | setosa |
| 4.7     | 3.2   | 1.3   | 0.2   | setosa |
| 4.6     | 3.1   | 1.5   | 0.2   | setosa |
| 5.0     | 3.6   | 1.4   | 0.2   | setosa |
| 5.4     | 3.9   | 1.7   | 0.4   | setosa |
| 4.6     | 3.4   | 1.4   | 0.3   | setosa |
| 5.0     | 3.4   | 1.5   | 0.2   | setosa |
| 4.4     | 2.9   | 1.4   | 0.2   | setosa |
| 4.9     | 3.1   | 1.5   | 0.1   | setosa |
| 5.4     | 3.7   | 1.5   | 0.2   | setosa |
| 4.8     | 3.4   | 1.6   | 0.2   | setosa |
| 4.8     | 3.0   | 1.4   | 0.1   | setosa |
| 4.3     | 3.0   | 1.1   | 0.1   | setosa |
| 5.8     | 4.0   | 1.2   | 0.2   | setosa |

Merging can also be turned off entirely with `merge = FALSE`, or aimed
at specific header rows using ordinary R subscripts, numbered from the
top down - `merge = 1:2` for the top two rows, or `merge = -3` for
everything except the third. Since `merge` works a row at a time, a row
that needs some of its repeated cells merged but not others should be
left out of `merge` and spanned with
[`flextable::merge_at()`](https://davidgohel.github.io/flextable/reference/merge_at.html)
instead.

Another common way you may want to apply headers is by using your
variable labels. By default, clintable will respect this. Furthermore,
the same spanning can be achieved as well. Let’s consider another
example:

``` r

iris2 <- iris
attr(iris2$Sepal.Length, "label") <- "Flower||Sepal||Length"
attr(iris2$Sepal.Width, "label") <- "Flower||Sepal||Width"
attr(iris2$Petal.Length, "label") <- "Flower||Petal||Length"
attr(iris2$Petal.Width, "label") <- "Flower||Petal||Width"

clintable(iris2) |>
  align(align = "center", part = "header") |>
  align(align = "center", part = "body")
```

| Flower |       |        |       |        |
|--------|-------|--------|-------|--------|
| Sepal  |       | Petal  |       |        |
| Length | Width | Length | Width |        |
| 5.1    | 3.5   | 1.4    | 0.2   | setosa |
| 4.9    | 3.0   | 1.4    | 0.2   | setosa |
| 4.7    | 3.2   | 1.3    | 0.2   | setosa |
| 4.6    | 3.1   | 1.5    | 0.2   | setosa |
| 5.0    | 3.6   | 1.4    | 0.2   | setosa |
| 5.4    | 3.9   | 1.7    | 0.4   | setosa |
| 4.6    | 3.4   | 1.4    | 0.3   | setosa |
| 5.0    | 3.4   | 1.5    | 0.2   | setosa |
| 4.4    | 2.9   | 1.4    | 0.2   | setosa |
| 4.9    | 3.1   | 1.5    | 0.1   | setosa |
| 5.4    | 3.7   | 1.5    | 0.2   | setosa |
| 4.8    | 3.4   | 1.6    | 0.2   | setosa |
| 4.8    | 3.0   | 1.4    | 0.1   | setosa |
| 4.3    | 3.0   | 1.1    | 0.1   | setosa |
| 5.8    | 4.0   | 1.2    | 0.2   | setosa |

The underlying logic of this example is exactly the same as using
[`clin_column_headers()`](https://atorus-research.github.io/clinify/reference/clin_column_headers.md),
and in fact the same header building machinery is applied by default.
The difference is that here, to separate levels we use the delimiter
`||`. Note in this example how the spanning variable have also changed
so that `Flower` stretches over all four Petal and Sepal columns.

Headers built this way can have their merging adjusted too. Call
[`clin_column_headers()`](https://atorus-research.github.io/clinify/reference/clin_column_headers.md)
with nothing but the `merge` argument and the header text coming from
the labels is left as it is:

``` r

iris3 <- iris
attr(iris3$Sepal.Length, "label") <- "Flower||Sepal||Value"
attr(iris3$Sepal.Width, "label") <- "Flower||Sepal||Value"
attr(iris3$Petal.Length, "label") <- "Flower||Petal||Value"
attr(iris3$Petal.Width, "label") <- "Flower||Petal||Value"

clintable(iris3) |>
  clin_column_headers(merge = "spanners")
```

| Flower |       |       |       |        |
|--------|-------|-------|-------|--------|
| Sepal  |       | Petal |       |        |
| Value  | Value | Value | Value |        |
| 5.1    | 3.5   | 1.4   | 0.2   | setosa |
| 4.9    | 3.0   | 1.4   | 0.2   | setosa |
| 4.7    | 3.2   | 1.3   | 0.2   | setosa |
| 4.6    | 3.1   | 1.5   | 0.2   | setosa |
| 5.0    | 3.6   | 1.4   | 0.2   | setosa |
| 5.4    | 3.9   | 1.7   | 0.4   | setosa |
| 4.6    | 3.4   | 1.4   | 0.3   | setosa |
| 5.0    | 3.4   | 1.5   | 0.2   | setosa |
| 4.4    | 2.9   | 1.4   | 0.2   | setosa |
| 4.9    | 3.1   | 1.5   | 0.1   | setosa |
| 5.4    | 3.7   | 1.5   | 0.2   | setosa |
| 4.8    | 3.4   | 1.6   | 0.2   | setosa |
| 4.8    | 3.0   | 1.4   | 0.1   | setosa |
| 4.3    | 3.0   | 1.1   | 0.1   | setosa |
| 5.8    | 4.0   | 1.2   | 0.2   | setosa |

Note that after headers are applied, additional styling can be done. In
this case, we use the
[`flextable::align()`](https://davidgohel.github.io/flextable/reference/align.html)
function to change the alignment on both the table body and column
headers to center. Note that default styles are applied when the table
is written out or printed, so these might potentially override some
settings depending on how those functions are applied.

### Writing to DOCX

While printing the table during development helps you with the
development process, ultimately **{clinify}** lets you write the
document to a docx file. This was a primary reason why we wanted to
build on top of {flextable}; with {flextable} and {officer} we’re able
to have a great amount of control over how things are written
specifically written into the word document. We try to make this process
rather seamless and comparable to the
[`print()`](https://rdrr.io/r/base/print.html) method. To write the
document out to docx, use the
[`write_clindoc()`](https://atorus-research.github.io/clinify/reference/write_clindoc.md)
function. Let’s revisit our table from before.

``` r

# Create a basic table
ct <- clintable(dat2) |>
  clin_page_by("page") |>
  clin_group_by(c("groups1", "groups2"), caption_by = "captions") |>
  clin_alt_pages(
    key_cols = c("mpg", "cyl", "hp"),
    col_groups = list(
      c("disp", "drat", "wt"),
      c("qsec", "vs", "am"),
      c("gear", "carb")
    )
  ) |>
  clin_column_headers(
    mpg = "Miles/(US) gallon",
    cyl = c("Number of cylinders"),
    disp = c("Displacement\n(cu.in.)"),
    hp = c("Gross horsepower"),
    drat = c("Span multiple pages", "Rear axle ratio"),
    wt = c("Span multiple pages", "Weight (1000 lbs)"),
    qsec = c("Span multiple pages", "1/4 mile time"),
    vs = c("Span multiple pages", "Engine\n(0 = V-shaped, 1 = straight)"),
    am = c("Span multiple pages", "Transmission\n(0 = automatic, 1 = manual)"),
    gear = c("Some Spanner", "Number of forward gears"),
    carb = c("Some Spanner", "Number of carburetors")
  ) |>
  clin_col_widths(mpg = .2, cyl = .2, disp = .15, vs = .15) |>
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
      c("One very long footnote full of text"),
      c("Two very long footnote full of text"),
      c("Three very long footnote full of text"),
      c("Four very long footnote full of text"),
      c("Five very long footnote full of text")
    )
  )

# Catch the officer::rdocx object itself
doc <- clindoc(ct)

# Write the clindoc
write_clindoc(doc, file = "example_table.docx")

# Alternately just write the clintable directly
write_clindoc(ct, file = "example_table.docx")
```

![](table.png)

### Why this framework?

So why **{clinify}**? As we explained earlier, the key idea is that
{flextable} and {officer} have so much of the functionality that’s
already needed - so **{clinify}** focuses on specific additional
features and streamline certain pieces to make the development of tables
more straightforward. One last point is that in building this package,
we also didn’t want to reinvent the wheel. Several other packages
coordinate with the {flextable} and {officer} ecosystem, such as
{rtables} or {gtsummary}. Our intent is that **{clinify}** can hopefully
work with these packages as well introduce some of this tedious
additional functionality.
