#' @title Vectorize functions
#'
#' @description Iterate a function and wrap a [dplyr::failwith()] around it.
#'
#' @importFrom magrittr %>%
#'
#' @param fun Function to be iterated
#' @param itens Character vector of inputs
#' @param ... Other parameters for `fun`
#' @param verbose Should dvec print the current `item` (if `TRUE`, shows a
#' message with probability `p`)?
#' @param p Probability of printing a message
#' 
#' @export
dvec <- function(fun, itens, ..., verbose = TRUE, p = .05) {
  f <- dplyr::failwith(tibble::data_frame(result = 'erro'), fun)
  tibble::data_frame(item = itens) %>%
    dplyr::distinct(item) %>%
    dplyr::group_by(item) %>%
    dplyr::do({
      if (stats::runif(1) < p && verbose) print(.$item)
      d <- f(.$item, ...)
      if (!tibble::has_name(d, 'result')) d$result <- 'OK'
      d
    }) %>%
    dplyr::ungroup()
}

#' @title Remove accentuation
#' 
#' @description Remove accented characters from strings converting them to
#' ASCII.
#' 
#' @param x A string vector
#' 
#' @return A version of `x` without non-ASCII characters
#' 
#' @export
rm_accent <- function(x) {
  if (.Platform$OS.type == 'unix') {
    gsub("`", "", iconv(x, to = "ASCII//TRANSLIT"))
  } else {
    gsub("`", "", iconv(x, from = 'latin1', to="ASCII//TRANSLIT"))
  }
}

#' @title Improved list of objects
#'
#' @description Elegantly list objects in a R session.
#'
#' @param pos Where to look for the object (see "Details" in [base::get()]'s
#' documentation)
#' @param pattern	An optional regular expression to match names ([utils::glob2rx()]
#' can be used to convert wildcard patterns to regular expressions)
#' @param order.by Sort by `"Size"` (default), `"Type"`, `"Rows"` or `"Columns"`
#' @param decreasing Should the sorting be decreasing?
#' @param head Should [utils::head()] function be used for printing?
#' @param n How many lines [utils::head()] function should show?
#'  
#' @references http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#'  
#'@export
lsos <- function (pos = 1, pattern, order.by = "Size",
                  decreasing=TRUE, head=TRUE, n=10) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

#' @title Add pipe template
#' 
#' @description Adds pipe template to package documentation.
#' 
#' @param pkg Package description (can be path or package name)
#' 
#' @export
use_pipe <- function(pkg = '.') {
  pkg <- devtools::as.package(pkg)
  devtools::use_package('magrittr', pkg = pkg)
  txt_pipe <- readLines(system.file('pipe-op.R', 
                                    package = 'abjutils'))
  cat(txt_pipe, file = paste0(pkg$path, '/R/utils.R'),
      append = TRUE, sep = '\n')
  devtools::document()
}

# Get rid of NOTEs
globalVariables(c(
  ".","item","object.size","%>%", "n_processo", "runif", "serial", "no_cd_code"))
