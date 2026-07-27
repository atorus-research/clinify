## Submission notes

This is a minor release (0.4.0). It adds control over parts of a table's
appearance that previously had to be reached by replacing clinify's styling
functions or by editing data before it was handed over: which rows of a column
header have their identical adjacent cells merged, the rule under a spanner
label, the row pitch and header spacing, how a table sits across the page, how
each title and footnote line is placed, and whether pre-formatted values are
rendered verbatim rather than reformatted. Titles and footnotes can also be
supplied as a data frame. See NEWS.md for the full list.

It also fixes several rendering faults, the most significant being that cell
merges were left in an invalid state when a table was paginated column wise,
which could drop header text from later pages or fail the write outright.

One narrow breaking change is documented in NEWS.md: because `merge` is now a
parameter of `clin_column_headers()`, a column named `merge` can no longer be
given a header through `...`. Column labels still work for such a column.

Organisations that copied `defaults_template.R` will want to pick up two
changes to it, both noted in NEWS.md.

## Test environments

* local: macOS, R 4.5.1
* GitHub Actions: Ubuntu 22.04 and ubuntu-latest (R-release and R-devel),
  macOS-latest (R-release), windows-latest (R-release)

## R CMD check results

0 errors | 0 warnings | 0 notes

Checked locally on macOS with R 4.5.1. The GitHub Actions environments listed
above are checked on every push; confirm they are green before submitting.

## Reverse dependencies

There are no reverse dependencies for this package.
