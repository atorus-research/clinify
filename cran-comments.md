## Submission notes

This is a patch release (0.4.1). It fixes a performance fault in `clindoc()`:
when a document was built from several tables, each table and page break was
written through its own {officer} append context, and every context serializes
and re-parses the whole document built so far. Build time therefore grew
quadratically with the number of tables, making documents of a few hundred
tables impractical to produce. All content is now written through a single
context. The document produced is unchanged. There are no API changes.

## Test environments

* local: macOS (arm64), R 4.5.1
* GitHub Actions: Ubuntu 22.04 and ubuntu-latest (R-release and R-devel),
  windows-latest (R-release)

## R CMD check results

0 errors | 0 warnings | 0 notes

Checked locally on macOS with R 4.5.1, and on every GitHub Actions
environment listed above.

The GitHub Actions macOS-latest job did not complete. Its new runner image
(macos-26-arm64) does not ship XQuartz, so the CRAN binary of the dependency
{gdtools} fails to load `/opt/X11/lib/libXrender.1.dylib` before clinify is
installed. This is a property of the runner, not of the package; the local
macOS check above passes.

## Reverse dependencies

There are no reverse dependencies for this package.
