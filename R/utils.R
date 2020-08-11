
#' @title Convert Chrome's Query String Parameters to a list
#'
#' @description To use this function, simply copy the Query String
#' Parameters returned by Chrome when analyzing the network flow of
#' a web page. Paste these QSPs into an R string with double quotes
#' (as you would to create any string) and pass it to
#' `chrome_to_body()`; the function will print to the console a
#' formatted command that creates a list with the QSPs. This list
#' works perfectly with [httr::GET()] and [httr::POST()] so that
#' you can easily reproduce a website's behavior.
#'
#' @param x A string with Chrome's Query String Parameters
#'
#' @seealso [httr::GET()], [httr::POST()]
#'
#' @export
chrome_to_body <- function(x) {
  x <- unlist(strsplit(x, "\n"))
  x_split <- stringr::str_split_fixed(x, "\\:(?=[^\\:]*$)", 2)
  x_split[, 2] <- stringr::str_trim(x_split[, 2])
  x_unite <- sprintf('"%s" = "%s"', x_split[, 1], x_split[, 2])
  x_unite <- paste(x_unite, collapse = ",\n")
  x_unite <- paste0("list(\n", x_unite, ")")
  cat(x_unite)
  invisible(x_unite)
}

#' Shortcut to write file to "data/" directory from a pipe
#'
#' @param x Object to write
#' @param name Name of the object (important when loading)
#' @param dir Directory where to save file
#'
#' @export
write_data <- function(x, name, dir = "data/") {
  assign(name, x)
  save(list = name, file = stringr::str_c(dir, "/", name, ".rda"))
  rm(name)

  return(x)
}

#' Extract file name without extension
#'
#' @param x Character vector of file paths
#'
#' @export
file_sans_ext <- function(x) {
  basename(tools::file_path_sans_ext(x))
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
  stringi::stri_trans_general(x, "Latin-ASCII")
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
#' @export
lsos <- function(pos = 1, pattern, order.by = "Size",
                 decreasing = TRUE, head = TRUE, n = 10) {
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
  if (!missing(order.by)) {
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  }
  if (head) {
    out <- utils::head(out, n)
  }
  out
}

#' Mirror of scales:::precision()
#'
#' @param x See scales:::precision()
#'
#' @export
precision <- function(x) {
  rng <- range(x, na.rm = TRUE)
  span <- if (zero_range(rng)) {
    abs(rng[1])
  } else {
    diff(rng)
  }
  if (span == 0) {
    return(1)
  }
  10^floor(log10(span))
}

# Mirror of scales::zero_range
zero_range <- function(x, tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1) {
    return(TRUE)
  }
  if (length(x) != 2) {
    stop("x must be length 1 or 2")
  }
  if (any(is.na(x))) {
    return(NA)
  }
  if (x[1] == x[2]) {
    return(TRUE)
  }
  if (all(is.infinite(x))) {
    return(FALSE)
  }
  m <- min(abs(x))
  if (m == 0) {
    return(FALSE)
  }
  abs((x[1] - x[2]) / m) < tol
}

#' Convert Brazilian currency values (text) to numeric
#'
#' @param x A currency vector. Ex: c("R$ 10.000,00", "R$ 123,00")
#'
#' @export
reais <- function(x) {
  x %>%
    stringr::str_remove("R\\$") %>%
    stringr::str_remove(".") %>%
    stringr::str_replace_all(",", "\\.") %>%
    as.numeric()
}

# Get rid of NOTEs
globalVariables(c(
  ".", "item", "object.size", "%>%", "n_processo", "runif", "serial", "no_cd_code", "warn",
  "output", "input", "result", "value", "id"
))
