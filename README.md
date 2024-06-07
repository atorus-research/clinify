
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **clinify**

<!-- badges: start -->
<!-- badges: end -->

**clinify** is direct extension of the
[**flextable**](https://davidgohel.github.io/flextable/) and
[**officer**](https://davidgohel.github.io/officer/) packages, aimed at
supplementing some functionality and simplifying some common tasks in
the creation of clinical tables, listings and figures.

## Installation

You can install the development version of **clinify** like so:

``` r
# Install the development version:
devtools::install_github("https://github.com/atorus-research/clinify.git", ref="development")
```

## Motivation

There’s a large assortment of table packages available, and there are
many that are specifically catered to clinical reporting. For many
organizations, one of these packages may very well be a great choice,
but there are a couple of key motivators we’ve found that lead drove us
to start writing **clinify**:

- Clinical output standards likely exist within an organization, and
  changing those standards may not be an option
- It’s very likely that certain outputs will require that you deviate
  from those standards, so flexibility it necessary, sometimes in highly
  nuanced ways
- Organizations like Contract Research Organizations (CROs) may have to
  adapt to multiple clients’ reporting standards, which likely don’t
  look the same
- Regardless of the situation, reuse and repeatability of a given
  configuration is critical, and changes in configuration must be easily
  implemented.

Instead of building a new package from the ground up, **clinify** aims
to extend what we see as a best choice for the situation at hand, adding
new functionality where necessary and streamlining common tasks to make
them more efficient for programmers to implement. We chose **flextable**
for a two key reasons:

- **flextable** already offers most of the functionality that we’re
  looking for, particularly with the pairing of **officer**. For
  example, word documents have to be a first class priority of output
  support.
- While creating a table is the focus, the output tends to be more than
  just a table. We need the capability to modify the underlying document
  as a whole, which is another place that **officer** is truly
  necessary.

## Design Philosophy

Here are some key principles we’re using in building **clinify**:

- **clinify** objects should inherit from an underlying **flextable** or
  **officer** object
- **clinify** functionality must not interfere with **flextable** or
  **officer** functionality, i.e. **flextable** or **officer** functions
  called should operate without error.
