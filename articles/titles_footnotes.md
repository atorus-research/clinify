# Titles and Footnotes

A specific thing that **{clinify}** helps users with is the application
of titles and footnotes to a document. In clinical tables, it’s common
that you need to use the header and the footer of the word document to
apply titles and footnotes. There are several functions available within
**{clinify}** to help you here.

## Titles and Footnotes

Let’s look at an example:

``` r

library(clinify)

clintable(mtcars) |>
  clin_add_titles(
    list(
      c("Left", "Page {PAGE} of {NUMPAGES}"),
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
```

|                 |           |
|-----------------|-----------|
| Left            | Page of   |
| Just the middle |           |

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

|                    |                               |
|--------------------|-------------------------------|
| Here's a footnote. | 13:07 Saturday, July 25, 2026 |

Both
[`clin_add_titles()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
and
[`clin_add_footnotes()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
take a list of character vectors. Each element of the list will be
applied as a row of the title or footnote. The number of elements of the
list determines how those elements are aligned:

- For a single element, in the titles it will be aligned center and in a
  footnote it will be bound left.
- Two elements will bind left and right

Note the text field `"Page {PAGE} of {NUMPAGES}"`. **{clinify}** has a
special helper function
[`clin_replace_pagenums()`](https://atorus-research.github.io/clinify/reference/clin_replace_pagenums.md)
that will find cells within a flextable containing `{PAGE}` or
`{NUMPAGES}` and replace them with the word auto field for current and
total pages respectively. By default within **{clinify}** this actual
takes places during the default styling functions (see
[`vignette("defaults")`](https://atorus-research.github.io/clinify/articles/defaults.md)).
Note that when you print to HTML, the `{PAGE}` and `{NUMPAGES}` portions
of the string will appear blank.

Another option for setting titles and footnotes is to create your own
flextable object.

``` r

clintable(mtcars) |>
  clin_add_titles(ft = flextable::flextable(head(iris, 2)))
```

| Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species |
|--------------|-------------|--------------|-------------|---------|
| 5.1          | 3.5         | 1.4          | 0.2         | setosa  |
| 4.9          | 3.0         | 1.4          | 0.2         | setosa  |

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

While this particular example is obscure, this opens up the possibility
to specifically craft your own title or footnote.

## Footnote Pages

Another circumstance that **{clinify}** allows for is the creation of a
specific footnote pages. This can be used in circumstances where there
are too many required footnotes for the table, and rather than putting
in the footer you can place them into their own specific preceding page
of the table itself. Also, note in this example that if you want to add
a single left aligned title, you can provide duplicate text, which keeps
the left alignment and merges the duplicate values.

``` r

clintable(mtcars) |>
  clin_add_titles(
    list(
      c("Left", "Left"),
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
```

|                 |     |
|-----------------|-----|
| Left            |     |
| Just the middle |     |

|                                       |     |
|---------------------------------------|-----|
| One very long footnote full of text   |     |
| Two very long footnote full of text   |     |
| Three very long footnote full of text |     |
| Four very long footnote full of text  |     |
| Five very long footnote full of text  |     |

|                    |                               |
|--------------------|-------------------------------|
| Here's a footnote. | 13:07 Saturday, July 25, 2026 |

|                 |     |
|-----------------|-----|
| Left            |     |
| Just the middle |     |

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

|                    |                               |
|--------------------|-------------------------------|
| Here's a footnote. | 13:07 Saturday, July 25, 2026 |

1

2

The interface to
[`clin_add_footnote_page()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
is identical to
[`clin_add_titles()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md)
and
[`clin_add_footnotes()`](https://atorus-research.github.io/clinify/reference/add_titles_footnotes.md),
so the same capabilities exist for this function as well.
