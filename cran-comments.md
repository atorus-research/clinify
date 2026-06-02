## Submission notes

This is a patch release (0.3.1) that fixes the R CMD check ERROR reported for
clinify 0.3.0 on r-devel-linux-x86_64-fedora-gcc.

The failure was in a unit test that compared {flextable} `autofit()` row
heights for exact equality. Those heights are estimated from font metrics and
vary across platforms, which produced a single-row mismatch (0.433 vs 0.422)
on r-devel-linux-x86_64-fedora-gcc only. The test no longer asserts on those
platform-dependent dimensions; it still verifies table data, content, spans,
styles, and headers. This release also brings compatibility with
{officer} (>= 0.7.0) and several styling/slicing fixes; see NEWS.md.

## Test environments

* local: macOS, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: "checking for future file timestamps ... unable to verify current
  time" is produced because the local check environment has no network access
  to verify the current time. It does not occur on CRAN and does not indicate
  a package problem.

## Reverse dependencies

There are no reverse dependencies for this package.
