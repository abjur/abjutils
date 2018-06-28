
#' @title Convert Chrome's Query String Parameters to a list
#' 
#' @description To use this function, simply copy the Query String
#' Parameters returned by Chrome when analysing the network flow of
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
  x_split[,2] <- stringr::str_trim(x_split[,2])
  x_unite <- sprintf('"%s" = "%s"', x_split[, 1], x_split[, 2])
  x_unite <- paste(x_unite, collapse = ",\n")
  x_unite <- paste0("list(\n", x_unite, ")")
  cat(x_unite)
  invisible(x_unite)
}

#' Extract file name without extension
#'
#' @param x Character vector of file paths
#' 
#' @export
file_sans_ext <- function(x) {
  basename(tools::file_path_sans_ext(x))
}

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

# Get number of available cores
get_cores <- purrr::partial(future::availableCores, constraints = "multicore")

#' @title Verbose, parallel, and safe map-like
#'
#' @description Using the same argument notation as [purrr::map()], this function
#' iterates over a list of inputs `.x`, applying `.f` to each element. It
#' returns a tibble with the input, whether the function returned an error
#' and the output.
#'
#' @importFrom magrittr %>%
#'
#' @param .x A list or atomic vector
#' @param .f A function, formula, or atomic vector (see [purrr::map()])
#' @param ... Other parameters passed on to [furrr::future_map()]
#' @param .cores Number of cores to use when multiprocessing
#' @param .progress Whether or not to display progress
#' @param .flatten Whether or not to return only the output of the
#' function (replaces errors with `NA`)
#' 
#' @return A tibble with 3 columns: input, return, and output
#' 
#' @export
pvec <- function(.x, .f, ..., .cores = get_cores(), .progress = TRUE, .flatten = FALSE) {
  
  # Preserve execution plan
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  
  # Set execution plan to multicore
  future::plan(future::multicore, workers = .cores)
  
  # Capture function side-effects
  .f <- purrr::safely(purrr::as_mapper(.f))
  
  # Run future map
  out <- furrr::future_map(.x, .f, ..., .progress = .progress)
  
  # Process output
  pout <- out %>%
    purrr::map(purrr::compact) %>%
    purrr::flatten() %>%
    tibble::tibble(id = seq_along(.x), return = names(.), output = .)
  
  # Flatten results if necessary
  if (.flatten) {
    pout <- pout %>%
      dplyr::mutate(
        output = dplyr::if_else(return == "error", list(NA), output)) %>%
      tidyr::unnest() %>%
      dplyr::pull(output)
  }
  
  return(pout)
}

#' @title Vectorized, parallel, safe and verbose function factory
#'
#' @description Wraps a function so that iterating over a set of inputs is
#' easily parallelizable, and interruption-free.
#'
#' @param .f Function to be wrapped
#' @param p Probability of function printing the index of the input it's
#' currently processing (if `cores > 1`)
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
    .f_ <- purrr::safely(.f)
    
    # Create "closure" that carries .f_ and p
    .c <- function(x, i) {
      
      # Print current index and apply function
      if (runif(1) < p & cores > 1) { print(i) }
      d <- .f_(x)
      
      # Handle error messages
      if (!is.null(d$error)) {
        d <- dplyr::tibble(result = as.character(d$error))
      } else {
        d <- d$result
      }
      
      # Convert into tibble if necessary
      if (!is.data.frame(d)) { d <- dplyr::tibble(output = list(d)) }
      if (!tibble::has_name(d, "result")) { d$result <- "Success" }
      
      return(d)
    }
    
    if (cores > 1) {
      
      # Create cluster (and prepare to free it)
      cl <- parallel::makeCluster(cores, outfile = "")
      on.exit(parallel::stopCluster(cl))
      
      # Run function in parallel
      out <- parallel::clusterMap(cl, .c, x = .x, i = seq_along(.x))
    }
    else {
      
      # Create progress bar
      pb <- progress::progress_bar$new(total = length(.x))
      
      # Loop over .x
      out <- list()
      for (i in seq_along(.x)) {
        pb$tick()
        out[[i]] <- .c(.x[[i]], i)
      }
    }
    
    # Bind rows and add input column
    out <- dplyr::bind_rows(out)
    out <- dplyr::bind_cols(tibble::tibble(input = .x), out)
    
    # Unnest if possible
    if (all(names(out) == c("input", "output", "result"))) {
      if (all(lengths(out$output) == 1)) {
        out <- out %>%
          tidyr::unnest(output) %>%
          dplyr::select(input, output, result)
      }
    }
    
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

#' Convert brazilian currency values (text) to numeric
#' 
#' @param x A currency vector. Ex: c("R$ 10.000,00", "R$ 123,00")
#' 
#' @export
reais <- function(x) {
  x %>% 
    str_remove("R\\$", "") %>% 
    str_remove(".", "") %>% 
    str_replace_all(",", "\\.") %>% 
    as.numeric()
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
  ".","item","object.size","%>%", "n_processo", "runif", "serial", "no_cd_code", "warn",
  "output", "input", "result", "value", "id"))
