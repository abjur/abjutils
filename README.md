abjutils
========

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/abjur/abjutils?branch=master&svg=true)](https://ci.appveyor.com/project/abjur/abjutils/branch/master)
[![Travis-CI Build Status](https://travis-ci.org/abjur/abjutils.svg?branch=master)](https://travis-ci.org/abjur/abjutils)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/abjutils)](https://cran.r-project.org/package=abjutils)

## Overview

`abjutils` is a toolkit with some useful functions widely used by the Brazilian
Jurimetrics Association. Most functions help deal with lawsuit identification
numbers (unified by CNJ, the National Council of Justice), while the rest are
supposed to help ASCII-compliance and other formatting issues.

To install this package, simply run:

```r
# To install CRAN version
install.packages("abjutils")

# To install GitHub (dev) version
install.packages("devtools")
devtools::install_github("abjur/abjutils")
```

## Usage

`abjutils` has many functions, but for regular users the most useful ones are
listed below

- `rm_accent`: removes diacritics from a string

- `escape_unicode`: replaces accented characters by their unicode-escaped values
(also an add-in)

## Citations

To cite this package, use `citation("abjutils")`:

```
To cite package ‘abjutils’ in publications use:

  Associacao Brasileira de Jurimetria (2020). abjutils:
  Useful Tools for Jurimetrical Analysis Used by the
  Brazilian Jurimetrics Association. R package version
  0.0.1. https://github.com/abjur/abjutils

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {abjutils: Useful Tools for Jurimetrical Analysis Used by the Brazilian Jurimetrics Association},
    author = {Associacao Brasileira de Jurimetria},
    year = {2020},
    note = {R package version 0.3.0},
    url = {https://github.com/abjur/abjutils},
  }
```
