#' Vectorize functions
#'
#' Iterate a function and wrap a dplyr::failtwith around it.
#'
#' @importFrom magrittr %>%
#'
#' @param fun function to be iterated.
#' @param itens character vector of inputs.
#' @param ... outros other parameters for \code{fun}
#' @param verbose should dvec print the current "item"?
#' if \code{TRUE} (default) shows a message with probability p.
#' @param p probability of printing a message. Only meaningful when verbose is 
#'   \code{TRUE}.
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

#' Remove accentuation
#' 
#' Remove accented characters from strings converting them to ASCII.
#' 
#' @param x A string vector
#' 
#' @return A version of x without non-ASCII characters.
#' 
#' @export
rm_accent <- function(x) {
  if (.Platform$OS.type == 'unix') {
    gsub("`", "", iconv(x, to = "ASCII//TRANSLIT"))
  } else {
    gsub("`", "", iconv(x, from = 'latin1', to="ASCII//TRANSLIT"))
  }
}

#'Improved list of objects
#'
#'Elegantly list objects in a R session.
#'
#'@param pos where to look for the object (see "Details" in base::get 
#'  documentation)
#'@param pattern	an optional regular expression. Only names matching pattern are
#'  returned. glob2rx can be used to convert wildcard patterns to regular 
#'  expressions.
#'@param order.by how should the list objects be sorted? Assume one of the 
#'  following values:  "Type", "Size" (default), "Rows" or "Columns".
#'@param decreasing should the sorting be decreasing? Skippable parameter.
#'@param head should the "head" function be used for printing? TRUE by default.
#'@param n how many lines the "head" function should show? (only meaningful when
#'  head = TRUE) 10 by default.
#'  
#'@section Credit:
#' Taken from http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#'  
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