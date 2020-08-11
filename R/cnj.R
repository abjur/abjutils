
#' @title Calculate digits for Brazilian lawsuit identification numbers
#'
#' @description Returns the check digit of a lawsuit numbers in the format
#' unified by the Brazilian National Council of Justice.
#'
#' @param num Ordered digits of the lawsuit number (including 0's) excluding
#' the check digit
#' @param build Whether or not the function return the complete lawsuit
#' number (or only the check digits)?
#'
#' @return The check digits or the complete identification number
#'
#' @examples
#' {
#'   calc_dig("001040620018260004", build = TRUE)
#'   calc_dig("001040620018260004", build = FALSE)
#' }
#' @export
calc_dig <- function(num, build = FALSE) {
  lengths <- stringr::str_length(num)

  if (max(lengths) != 18 | min(lengths) != 18) {
    stop("Lawsuit IDs without check digits should have 18 numerical digits.")
  }

  NNNNNNN <- substr(num, 1L, 7L)
  AAAA <- substr(num, 8L, 11L)
  JTR <- substr(num, 12L, 14L)
  OOOO <- substr(num, 15L, 18L)
  n1 <- sprintf("%02d", as.numeric(NNNNNNN) %% 97)
  n2 <- sprintf("%02d", as.numeric(sprintf("%s%s%s", n1, AAAA, JTR)) %% 97)
  n3 <- sprintf("%02d", 98 - ((as.numeric(sprintf("%s%s", n2, OOOO)) * 100) %% 97))
  dig <- n3
  if (build) {
    return(sprintf("%s%s%s", substr(num, 1, 7), dig, substr(num, 8, 18)))
  }
  return(dig)
}

#' @title Validate check digits for Brazilian lawsuits identification
#' number
#'
#' @description Verifies if a check digit is correct
#'
#' @param num String containing the complete lawsuit number
#'
#' @return Whether or not the check digit is well calculated
#'
#' @examples
#' {
#'   check_dig("0005268-75.2013.8.26.0100")
#' }
#' @export
check_dig <- function(num) {
  num <- stringr::str_replace_all(num, "[.-]", "")

  if (stringr::str_length(num) != 20) {
    warning("Complete docket numbers should have 20 numerical digits.")
    return(FALSE)
  }

  num_no_dig <- stringr::str_c(substr(num, 1L, 7L), substr(num, 10L, 20L))

  num_with_dig <- calc_dig(num_no_dig, build = TRUE)

  return(identical(num_with_dig, num))
}

#' @title Validate check digits for Brazilian lawsuits identification
#' number on vectors.
#'
#' @description Verifies if a check digit is correct
#'
#' @param num A vector containing strings with the complete lawsuit number
#'
#' @return Whether or not the check digit is well calculated
#'
#' @examples
#' {
#'   check_dig_vet(c("0005268-75.2013.8.26.0100", "0004122-85.2010.6.16.0100"))
#' }
#' @export
check_dig_vet <- function(num) {
  purrr::map_lgl(num, abjutils::check_dig)
}


#' @title Validate Brazilian lawsuits identification number on vectors.
#'
#' @description Verifies if a Brazilian lawsuit identification is a cnj number.
#'
#' @param cnj A vector containing strings with the complete lawsuit number
#'
#' @return Whether or not the check digit is well calculated
#'
#' @export
verify_cnj <- function(cnj) {
  nprocesso2 <- dplyr::if_else(is.na(cnj), "", clean_cnj(cnj))

  resp <- dplyr::case_when(
    nprocesso2 == "" ~ "vazio ou NA",
    stringr::str_length(clean_cnj(cnj)) > 20 ~ "> 20 digitos",
    !check_dig_vet(stringr::str_pad(dplyr::if_else(stringr::str_length(nprocesso2) > 20, str_sub(nprocesso2, end = 20), nprocesso2), 20, "left", "0")) ~ "dv invalido ou nao-cnj",
    T ~ "valido"
  )
  return(resp)
}

#' @title Extract different parts from lawsuit ID
#'
#' @description Given one or more lawsuit IDs, this function extracts one or more
#' parts of the IDs given the following correspondence:
#' \itemize{
#'   \item "N": number
#'   \item "D": verification digits
#'   \item "A": year
#'   \item "J": segment
#'   \item "T": court
#'   \item "O": origin
#'   \item "": all of the above
#' }
#'
#' @param id One or more lawsuit IDs
#' @param parts String or string vector with desired parts (see **description**)
#'
#' @examples
#' \dontrun{
#'   extract_parts("001040620018260004", "N")
#'   extract_parts("001040620018260004", c("N", "A", "O"))
#' }
#' @export
extract_parts <- function(id, parts = "") {

  # Handle parts
  parts <- unique(parts)
  if (any(parts == "")) {
    parts <- c("N", "D", "A", "J", "T", "O")
  }
  if (any(!(parts %in% c("N", "D", "A", "J", "T", "O")))) {
    stop("Invalid parts")
  }

  # Add verification digits to IDs that need them
  id <- id %>%
    clean_id() %>%
    purrr::modify_if(~ stringr::str_length(.x) == 18, calc_dig, build = TRUE) %>%
    unlist()

  # Extract parts for one ID
  get_parts <- function(id, parts) {

    # For every part, run str_sub()
    out <- c()
    for (part in parts) {

      # Get range for str_sub()
      range <- switch(part,
        "N" = list(1, 7),
        "D" = list(8, 9),
        "A" = list(10, 13),
        "J" = list(14, 14),
        "T" = list(15, 16),
        "O" = list(17, 20)
      )

      # Add new element to list
      out <- c(out, purrr::set_names(
        stringr::str_sub(id, range[[1]], range[[2]]), part
      ))
    }

    return(out)
  }

  # Handle vectorization
  purrr::map(id, get_parts, parts)
}

#' Remove separators from lawsuit IDs
#'
#' @param id One or more lawsuit IDs
#'
#' @export
clean_id <- function(id) {
  stringr::str_replace_all(id, pattern = "[\\-\\.]", replacement = "")
}

#' Add separators to lawsuit IDs
#'
#' @param id One or more lawsuit IDs
#'
#' @export
build_id <- function(id) {

  # Build one ID
  build <- function(id) {
    stringr::str_c(
      id[1], "-", id[2], ".", id[3], ".",
      id[4], ".", id[5], ".", id[6]
    )
  }

  # Handle vectorization
  purrr::map_chr(extract_parts(id), build)
}

#' @title Separate a lawsuit ID column into its parts
#'
#' @description Wrapper around [tidyr::separate()] that splits a column
#' with lawsuit IDs into 6 columns with its parts (see [extract_parts()]).
#' Note that the IDs must be built (see [build_id()]).
#'
#' @param data A data frame
#' @param col Column name or position (see [tidyr::separate()])
#' @param ... Other arguments passed on to [tidyr::separate()]
#'
#' @export
separate_cnj <- function(data, col, ...) {
  col <- rlang::enquo(col)
  tidyr::separate(
    data, rlang::UQ(col),
    into = c("N", "D", "A", "J", "T", "O"), sep = "[\\-\\.]", ...
  )
}

#' Regex pattern for finding lawsuit numbers
#'
#' @export
pattern_cnj <- function() {
  stringr::str_glue(
    "[0-9]{{3,7}}-?",
    "[0-9]{{2}}\\.?",
    "[0-9]{{4}}\\.?",
    "[0-9]{{1}}\\.?",
    "[0-9]{{2}}\\.?",
    "[0-9]{{4}}"
  ) %>% as.character()
}

#' @title Clean a cnj number.
#'
#' @description Remove all non-numeric character from a string
#'
#' @param x A string (cnj)
#'
#' @export
clean_cnj <- function(x) {
  stringr::str_replace_all(x, "[^0-9]", "")
}
