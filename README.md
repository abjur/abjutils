abjutils [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/abjur/abjutils?branch=master&svg=true)](https://ci.appveyor.com/project/abjur/abjutils/branch/master) [![Travis-CI Build Status](https://travis-ci.org/abjur/abjutils.svg?branch=master)](https://travis-ci.org/abjur/abjutils)
========

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
devtools::install_github("courtsbr/esaj")
```

## Usage

`abjutils` has many functions, but the most useful are listed below

- `rm_accent`: removes diacritics from a string

- `escape_unicode`: replaces accented characters by their unicode-escaped values
(also an add-in)

- `carefully`: a safe and protected vectorizer useful for things such as web scrapers

## Citations

To cite this package, use `citation("abjutils")`:

```
To cite package ‘abjutils’ in publications use:

  Julio Trecenti and Fernando Correa (2014). abjutils:
  Useful Tools for Jurimetrical Analysis Used by the
  Brazilian Jurimetrics Association. R package version
  0.0.1. https://github.com/abjur/abjutils

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {abjutils: Useful Tools for Jurimetrical Analysis Used by the Brazilian Jurimetrics Association},
    author = {Julio Trecenti and Fernando Correa},
    year = {2014},
    note = {R package version 0.0.1},
    url = {https://github.com/abjur/abjutils},
  }
```
