
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abjutils <a href='http://abjur.github.io/abjutils/'><img src='man/figures/logo.png' align="right" height="138.5" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/abjutils)](https://cran.r-project.org/package=abjutils)
[![R build
status](https://github.com/abjur/abjutils/workflows/R-CMD-check/badge.svg)](https://github.com/abjur/abjutils/actions)
<!-- badges: end -->

## Visão Geral

`{abjutils}` is a toolkit with some useful functions created by the
Brazilian Jurimetrics Association ([Associação Brasileira de
Jurimetria](https://abj.org.br/)).

Most functions help with identifying lawsuit IDs as specified by the
National Justice Council ([Conselho Nacional de
Justiça](https://www.cnj.jus.br/programas-e-acoes/numeracao-unica/documentos/)):
NNNNNNN-DD.AAAA.J.TR.OOOO. The rest helps with ASCII and other
formatting problems.

## Installation

You can install the most recent version of `{abjutils}` with:

``` r
# CRAN
install.packages("abjutils")

# GitHub (dev)
install.packages("remotes")
remotes::install_github("abjur/abjutils")
```

## Functions

| Function            | Description                                                 |
|---------------------|-------------------------------------------------------------|
| `build_id()`        | Add separators to ID                                        |
| `calc_dig()`        | Calculate verification digit of an ID                       |
| `carf_build_id()`   | Add digits to CARF ID                                       |
| `carf_calc_dig()`   | Calculate verification digit of a CARF ID                   |
| `carf_check_dig()`  | Check digits of a CARF ID                                   |
| `check_dig()`       | Check digits of an ID                                       |
| `check_dig_vet()`   | Check a verification digit vector                           |
| `chrome_to_body()`  | Convert POST parameters to a list                           |
| `clean_cnj()`       | Remove non-numeric characters from a string                 |
| `clean_id()`        | Remove separators from an ID                                |
| `escape_unicode()`  | Replace extended Latin characters with escaped Unicode      |
| `extract_parts()`   | Extract ID parts                                            |
| `file_sans_ext()`   | Extract filename without extension                          |
| `gather_subjects()` | Gather ESAJ subjects automatically                          |
| `lsos()`            | List objects in an R session                                |
| `pattern_cnj()`     | Regex pattern to find IDs                                   |
| `precision()`       | Apply precision scale                                       |
| `reais()`           | Convert BRL strings into numbers                            |
| `rm_accent()`       | Remove diacritics from a string                             |
| `sample_cnj()`      | Create a random sample of IDs                               |
| `separate_cnj()`    | Separate a column of IDs into 6 columns with its components |
| `tabela()`          | Create a contingency table of a vector                      |
| `test_fun()`        | Check if all arguments from a function are set              |
| `verify_cnj()`      | Check if ID conforms with CNJ’s standard                    |
| `write_data()`      | Write file to `data/`                                       |

## Usage

Example 1:

``` r
# Remove separators from ID
abjutils::clean_id(c("1025736-09.2014.8.26.0100","0043877-64.2012.8.26.0100","1013689-61.2018.8.26.0100"))
#> [1] "10257360920148260100" "00438776420128260100" "10136896120188260100"
```

Example 2:

``` r
# Extract components from ID
abjutils::extract_parts(c("1025736-09.2014.8.26.0100","0043877-64.2012.8.26.0100","1013689-61.2018.8.26.0100"))
#> [[1]]
#>         N         D         A         J         T         O 
#> "1025736"      "09"    "2014"       "8"      "26"    "0100" 
#> 
#> [[2]]
#>         N         D         A         J         T         O 
#> "0043877"      "64"    "2012"       "8"      "26"    "0100" 
#> 
#> [[3]]
#>         N         D         A         J         T         O 
#> "1013689"      "61"    "2018"       "8"      "26"    "0100"
```

Example 3:

``` r
# Remove diacritics from string
abjutils::rm_accent("acórdão")
#> [1] "acordao"
```

## Dependencies

`{abjutils}` requires R \>= 3.6.

## License

`{abjutils}` is licensed under [MIT + file
LICENSE](https://github.com/abjur/abjutils/blob/master/LICENSE)
