prettify_number <- function(d, number = T, percent = T, ...) {
  if (percent) {
    query <- "{fmt_p(x/sum(x))}"
  }

  if (number) {
    query <- sprintf("{fmt(x)} (%s)", stringr::str_glue(query))
  }

  d %>%
    dplyr::select(...) %>%
    dplyr::mutate_if(is.numeric, .funs = function(x) {
      stringr::str_glue(query)
    }) %>%
    dplyr::as_data_frame()
}

# adapted from plyr package
fmt <- function(x) {
  format(x,
    big.mark = ".", small.mark = ",",
    decimal.mark = ",",
    scientific = FALSE, trim = TRUE
  )
}

# adapted from plyr package
fmt_p <- function(x) {
  if (length(x) == 0) {
    return(character())
  }
  accuracy <- precision(x) / 100
  x <- round(x / accuracy) * accuracy
  x <- fmt(x * 100)
  paste0(x, "%")
}
