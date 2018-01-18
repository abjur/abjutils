#' @title Vectorize functions (DEPRECATED)
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
  
  warning("`dvec()` is deprecated; please use `abjutils::carefully()` instead.", call. = FALSE)
  
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

#' @title Vectorized, parallel, safe and verbose function factory
#'
#' @description Wraps a function so that iterating over a set of inputs is
#' easily parallelizable, and interruption-free.
#'
#' @param .f Function to be wrapped
#' @param p Probability of function printing the index of the input it's
#' currently processing
#' @param cores Number of cores to use when iterating over vectorized
#' inputs
#' 
#' @examples
#' \dontrun{
#' # Function that takes a string and pastes two other strings
#' # a its beginning and end respectivelly
#' pad <- function(str, b = "", a = "") { paste0(b, str, a) }
#' 
#' # Create wrapped version of pad() that executes over 4 cores,
#' # captures errors, and prints its current iteration with a
#' # probability of 50%
#' new_pad <- carefully(pad, p = 0.5, cores = 4)
#' 
#' # Execute new_pad() with some sample data
#' new_pad(c("asdf", "poiu", "qwer"), b = "0", a = "1")
#' }
#' 
#' @export
carefully <- function(.f, p = 0.05, cores = 1) {
  
  # New, wrapped function
  function(.x, ...) {
    
    # Wrap function
    .f <- purrr::lift_dl(.f, ...)
    .f_ <- purrr::possibly(.f, tibble::tibble(result = "ERROR"))
    
    # Create "closure" that carries .f_ and p
    .c <- function(x, i) {
      
      # Print current index and apply function
      if (runif(1) < p) { print(i) }
      d <- .f_(x)
      
      # Convert into tibble if necessary
      if (!is.data.frame(d)) { d <- dplyr::tibble(output = d) }
      if (!tibble::has_name(d, "result")) { d$result <- "OK" }
      
      return(d)
    }
    
    # Create cluster (and prepare to free it)
    cl <- parallel::makeCluster(cores, outfile = "")
    on.exit(parallel::stopCluster(cl))
    
    # Run function in parallel
    out <- parallel::clusterMap(cl, .c, x = .x, i = seq_along(.x))
    
    # Bind rows and add input column
    out <- dplyr::bind_rows(out)
    out <- dplyr::bind_cols(tibble::tibble(input = .x), out)
    
    return(out)
  }
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
    stringr::str_replace_all(iconv(x, to = "ASCII//TRANSLIT"), "[`'\"^~]", "")
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

#' Mirror of scales:::precision()
#' 
#' @param x See scales:::precision()
#' 
#' @export
precision <- function(x) {
  rng <- range(x, na.rm = TRUE)
  span <- if (scales::zero_range(rng)) 
    abs(rng[1])
  else diff(rng)
  if (span == 0) 
    return(1)
  10^floor(log10(span))
}
 
# #' @title Add a progress bar to any mapping function
# #' 
# #' @description This function wraps any function of the [purrr::map()] family,
# #' adding a progress bar to the iteration.
# #' 
# #' @param ..f A function, formula, or atomic vector (see [purrr::map()])
# #' 
# #' @examples { \dontrun {
# #' map_progress <- with_progress(purrr::map)
# #' map_progress(1:10, ~{ Sys.sleep(1); .x })
# #' }
# #' 
# #' @export
# with_progress <- function(..f) {
#   function(x, .f, ...) {
#     .f <- purrr::as_mapper(.f)
#     p <- progress::progress_bar$new(total = length(x))
#     fun <- function(.x, ...) { p$tick(); .f(.x, ...) }
#     ..f(x, fun, ...)
#   }
# }
#
# #' @title Add [purrr::possibly()] to any mapping function
# #' 
# #' @description This function wraps any function of the [purrr::map()] family,
# #' capturing side effects with [purrr::possibly()]. This is useful when only
# #' the side effects are desired as the returned values will be either `TRUE`
# #' or `FALSE` (if that iteration did/didn't work).
# #' 
# #' @param ..f A function, formula, or atomic vector (see [purrr::map()])
# #' 
# #' @export
# with_possibly <- function(..f) {
#   function(x, .f, ...) {
#     .f <- purrr::as_mapper(.f, ...)
#     .f_ <- purrr::possibly(function(.x, ...) { .f(.x, ...); TRUE }, FALSE, FALSE)
#     purrr::flatten_lgl(..f(x, .f_, ...))
#   }
# }
# 
# #' @title Add parallelism to any mapping function
# #' 
# #' @description This function wraps any function of the [purrr::map()] family,
# #' parallelizing its iterations with [parallel::mcmapply()]. Note that flattening
# #' doesn't work, so `purrr::map_***()` becomes only `purrr::map()`.
# #' 
# #' @param ..f A function, formula, or atomic vector (see [purrr::map()])
# #' 
# #' @export
# with_parallelism <- function(..f) {
#   function(x, .f, ...) {
#     .f <- purrr::as_mapper(.f, ...)
#     .f_ <- purrr::partial(..f, .f = .f)
#     purrr::flatten(
#       parallel::mcmapply(.f_, x, MoreArgs = ..., SIMPLIFY = FALSE))
#   }
# }
# 
# #' @title Add paralellism and [purrr::possibly()] to any mapping function
# #' 
# #' @description This function wraps any function of the [purrr::map()] family,
# #' running everything in parallel and capturing side effects with [purrr::possibly()].
# #' This is useful when only the side effects are desired as the returned values
# #' will be either `TRUE` or `FALSE` (if that iteration did/didn't work).
# #' 
# #' @param ..f A function, formula, or atomic vector (see [purrr::map()])
# #' 
# #' @export
# with_2p <- function(..f) {
#   with_parallelism(with_possibly(..f))
# }

# Get rid of NOTEs
globalVariables(c(
  ".","item","object.size","%>%", "n_processo", "runif", "serial", "no_cd_code", "warn"))
