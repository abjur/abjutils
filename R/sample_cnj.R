#' @title Generate sample Brazilian lawsuit identification numbers
#'
#' @description Returns a data frame containing a random sample of lawsuit
#' numbers distributed according to some regional and jurisdictional parameters.
#' The implementation supports both vector and scalar parameters, depending
#' whether or not the function should uniformly sample from a scope of lawsuit
#' numbers or one should define the parameters for each sample unit.
#'
#' @param n A non negative integer giving the number of codes to generate
#' @param foros One or more strings with 4 characters indicating the juridical
#' forum for the sampled codes
#' @param anos One or more strings with 4 characters indicating the distribution
#' years of the generated codes
#' @param orgao One or more strings with 1 character indicating the jurisdiction
#' of the sampled codes.
#' @param tr One or more strings with 1 character indicating the court of the
#' generated codes
#' @param first_dig The first digit of the lawsuit code (`"0"` by default and
#' sampled if `""`)
#' @param sample_pars Whether or not the parameters define the characteristics
#' of the codes
#' @param return_df Whether or not the function should return a data frame
#'
#' @return A data frame or a vector containing a random sample of lawsuits IDs
#'
#' @examples
#' {
#'   # sampling the parameters
#'   sample_cnj(3,
#'     foros = "0000",
#'     anos = "2015", orgao = 8, tr = 26,
#'     first_dig = "0", sample_pars = TRUE, return_df = FALSE
#'   )
#'
#'   sample_cnj(10,
#'     foros = c("0000", "0001"),
#'     anos = c("2014", "2015"), orgao = 8, tr = 26,
#'     first_dig = "0", sample_pars = TRUE, return_df = FALSE
#'   )
#'
#'   # not sampling the parameters
#'
#'   sample_cnj(3,
#'     foros = c("0000", "0001", "0002"),
#'     anos = c("2014", "2015", "2016"), orgao = rep(8, 3), tr = rep(26, 3),
#'     first_dig = "0", sample_pars = FALSE, return_df = FALSE
#'   )
#' }
#' @export
sample_cnj <- function(n, foros, anos, orgao, tr, first_dig = "0",
                       sample_pars = TRUE, return_df = TRUE) {

  # checks
  if (sample_pars) {
    foros <- build_params_list(foros, n)
    anos <- build_params_list(anos, n)
    orgao <- build_params_list(orgao, n)
    tr <- build_params_list(tr, n)
  } else {
    lengths <- sapply(list(foros, anos, orgao, tr), length)

    max_length <- max(lengths)

    logical_test <- max_length != min(lengths)

    if (logical_test) {
      stop("When sample_pars is FALSE, sample_cnj expects foros, anos, orgaos and tr with same length.")
    } else {
      if (max_length != 1 & (max_length != n)) {
        stop("When sample_pars is FALSE, sample_cnj expects parameters lengths to be equal to n.")
      }
    }
  }
  # end of checks

  serial_size <- ifelse(first_dig == "", 9, 8)

  # main code
  ret <- runif(n) %>%
    as.character() %>%
    stringr::str_sub(3, serial_size) %>%
    dplyr::data_frame() %>%
    stats::setNames("serial") %>%
    dplyr::mutate(
      no_cd_code = sprintf(
        "%s%s%s%s%s%s", first_dig,
        serial, anos, orgao, tr, foros
      ),
      n_processo = calc_dig(no_cd_code, build = T)
    ) %>%
    dplyr::select(n_processo)

  # df or vec
  if (return_df) {
    return(ret)
  } else {
    return(with(ret, n_processo))
  }
}

build_params_list <- function(x, n) {
  if (length(x) > 1) {
    sample(as.character(x), n, replace = T)
  } else {
    rep(x, n)
  }
}
