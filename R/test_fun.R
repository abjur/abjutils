
#' @title Tests a function by checking if its arguments are declared
#'
#' @description This function verifies whether all of the arguments of another
#' function already have assigned values. If an argument has a default value
#' but there isn't a corresponding variable, it creates that variable.
#'
#' @param f A function
#' @param force_default Whether or not to assign the default value to arguments
#' that already have assigned values
#'
#' @examples
#' \dontrun{
#' f <- function(a, b = 3) {
#'   a * b
#' }
#'
#' test_fun(f)
#' a
#' b
#'
#' b <- 5
#' test_fun(f)
#' a
#' b
#'
#' test_fun(f, TRUE)
#' a
#' b
#'
#' a <- 2
#' test_fun(f)
#' a
#' b
#' }
#'
#' @export
test_fun <- function(f, force_default = FALSE) {
  args <- names(formals(f))
  vals <- formals(f)

  does_exist <- purrr::map_lgl(args, exists, envir = rlang::env_parent())

  for (i in seq_along(args)) {
    if (does_exist[[i]]) {
      if (force_default) {
        assign(args[[i]], eval(vals[[i]]), envir = rlang::env_parent())
      }
    }
    else if (!rlang::is_missing(vals[[i]])) {
      assign(args[[i]], eval(vals[[i]]), envir = rlang::env_parent())
      does_exist[[i]] <- TRUE
    }
    else {
      message(paste0("Argument named '", args[[i]], "' needs a value!"))
    }
  }

  invisible(does_exist)
}
