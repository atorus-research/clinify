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
* GitHub Actions: Ubuntu 22.04 and ubuntu-latest (R-release and R-devel),
  macOS-latest (R-release), windows-latest (R-release)

## R CMD check results

0 errors | 0 warnings | 1 note

The package checks cleanly (0 errors, 0 warnings) on all of the GitHub Actions
environments listed above, including R-devel on Linux. The only NOTE appears
locally: "checking for future file timestamps ... unable to verify current
time", which is caused by the local check machine having no network access to
verify the current time; it does not occur on CRAN.

## Reverse dependencies

There are no reverse dependencies for this package.
