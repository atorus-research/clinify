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

  

.cl-ef56041a{}.cl-ef4e5292{font-family:'Courier New';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ef518692{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ef51869c{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ef51869d{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ef51b842{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ef51b84c{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ef51b856{width:4.125in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Left
```

Right

Just the middle

|                                      |                                              |
|--------------------------------------|----------------------------------------------|
| Use when you have a lot of footnotes | And you don't want to put them on every page |

|                    |                                  |
|--------------------|----------------------------------|
| Here's a footnote. | 19:55 Thursday, January 08, 2026 |

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

|                    |                                  |
|--------------------|----------------------------------|
| Here's a footnote. | 19:55 Thursday, January 08, 2026 |

1

2
