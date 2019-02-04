
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
      return = names(.), output = .
    )

  # Flatten results if necessary
  if (.flatten) {
    n_error <- length(pout$return[pout$return == "error"])
    if (n_error > 0) {
      warning(
        "Since '.flatten = TRUE', a total of ", n_error,
        " errors are being ignored",
        call. = FALSE
      )
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

# Get number of available cores
get_cores <- purrr::partial(future::availableCores, constraints = "multicore")

# Import of future_options()
future_options <- furrr::future_options
