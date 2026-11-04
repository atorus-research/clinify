## Submission notes

This is a patch release (0.4.1). It fixes a performance fault in `clindoc()`:
when a document was built from several tables, each table and page break was
written through its own {officer} append context, and every context serializes
and re-parses the whole document built so far. Build time therefore grew
quadratically with the number of tables, making documents of a few hundred
tables impractical to produce. All content is now written through a single
context. The document produced is unchanged. There are no API changes.

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
