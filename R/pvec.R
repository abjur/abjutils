
#' @title Verbose, parallel, and safe map-like
#'
#' @description Using the same argument notation as [purrr::map()], this function
#' iterates over a list of inputs `.x`, applying `.f` to each element. It
#' returns a tibble with the id, whether the function returned an error
#' and the output.
#'
#' @importFrom magrittr %>%
#'
#' @param .x A list or atomic vector
#' @param .f A function, formula, or atomic vector (see [purrr::map()])
#' @param ... Other parameters passed on to `.f`
#' @param .cores Number of cores to use when multiprocessing
#' @param .progress Whether or not to display progress
#' @param .flatten If `TRUE`, the errors are filtered from the output,
#' and the returned object is flattened (a vector, a list, or a tibble)
#' @param .options Options passed on to [furrr::future_map()]
#' ([furrr::future_options()] by default)
#' 
#' @seealso [purrr::map()], [furrr::future_map()], [furrr::future_options()]
#' 
#' @return A tibble with 3 columns: input, return, and output
#' @export
pvec <- function(.x, .f, ..., .cores = get_cores(), .progress = TRUE, .flatten = FALSE, .options = future_options()) {
  
  # Preserve execution plan
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  
  # Set execution plan to multicore
  future::plan(future::multicore, workers = .cores)
  
  # Capture function side-effects
  .f <- purrr::safely(purrr::as_mapper(.f))
  
  # Run future map
  out <- furrr::future_map(.x, .f, ..., .progress = .progress, .options = .options)
  
  # Compact with care
  compact_ <- function(x) {
    if (is.null(x[[1]]) && is.null(x[[2]])) {
      return(list(result = NULL))
    }
    else {
      return(purrr::compact(x))
    }
  }
  
  # Process output
  pout <- out %>%
    purrr::map(compact_) %>%
    purrr::flatten() %>%
    tibble::tibble(
      id = purrr::`%||%`(names(.x), seq_along(.x)),
      return = names(.), output = .)
  
  # Flatten results if necessary
  if (.flatten) {
    
    n_error <- length(pout$return[pout$return == "error"])
    if (n_error > 0) {
      warning(
        "Since '.flatten = TRUE', a total of ", n_error,
        " errors are being ignored", call. = FALSE)
    }
    
    pout <- pout %>%
      dplyr::filter(return != "error") %>%
      dplyr::select(-return) %>% 
      tidyr::unnest()
    
    if (ncol(pout) == 1) {
      pout <- dplyr::pull(pout, output)
    }
  }
  
  return(pout)
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

# Import of future_options()
future_options <- furrr::future_options

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
