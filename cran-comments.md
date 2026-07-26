## Submission notes

This is a minor release (0.4.0). It adds a `merge` argument to
`clin_column_headers()` so that automatic merging of identical, adjacent
column header cells can be limited to chosen header rows, and it fixes cell
merges being left in an invalid state when a table is paginated column wise.
See NEWS.md.

One narrow breaking change is documented in NEWS.md: because `merge` is now a
parameter of `clin_column_headers()`, a column named `merge` can no longer be
given a header through `...`.

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
