verify_length <- function(val) {
  year <- substr(val, 12, 13)
  len <- ifelse(year == "20", 15, 13)
  wrong_len <- which(nchar(val) != len)
  if (length(wrong_len) > 0) {
    examples <- paste(utils::head(val[wrong_len], 10), sep = ", ")
    tip <- "\n\nDid you remove the check digits?"
    stop(paste0("Wrong lengths: ", examples, "...", tip))
  }
}

#' @title Calculate check digit for CARF
#' 
#' @description Returns the check digit of a CARF number or full number with
#' the check digit.
#' 
#' @param id Lawsuit number (including trailling zeros), excluding
#' the check digit.
#' @param build Whether or not the function return the complete number
#' (or only the check digits)?
#' @param verify Verify if number is well formed (gives error if it's not)
#' 
#'
#' @return The check digits or the complete identification number
#'   
#' @examples {
#' carf_calc_dig("10120.008427/2003", build = TRUE)
#' carf_calc_dig("15374.002430/99", build = FALSE)
#' carf_calc_dig(c("101200084272003", "1537400243099"))
#' \donttest{
#' carf_calc_dig("10766.000511/96-12")
#' }
#' }
#' 
#' @export
carf_calc_dig <- function(id, build = FALSE, verify = TRUE) {
  val <- gsub("[^0-9]", "", id)
  val <- substr(val, 1, nchar(val))
  if (verify) verify_length(val)
  calc_dv_one <- function(val) {
    len <- nchar(val)
    index <- seq(len + 1, 2)
    all_values <- as.numeric(unlist(strsplit(val, "")))
    weighted_sum <- sum(all_values * index)
    dv <- 11 - (weighted_sum %% 11)
    if (dv == 10) dv <- 0
    if (dv == 11) dv <- 1
    as.character(dv)
  }
  dv1 <- purrr::map_chr(val, calc_dv_one)
  dv2 <- purrr::map_chr(paste0(val, dv1), calc_dv_one)
  dv <- paste0(dv1, dv2)
  if (build) {
    paste0(val, dv)
  } else {
    dv
  }
}

#' @title Validate check digits for Brazilian lawsuits identification
#' number
#' 
#' @description Verifies if a check digit is correct
#' 
#' @param id String containing the complete lawsuit number
#' 
#' @return Whether or not the check digit is well calculated
#'   
#' @examples {
#' carf_check_dig("10120.008427/2003-02")
#' carf_check_dig(c("10120008427200302", "10766.000511/96-12"))
#' }
#' @export
carf_check_dig <- function(id) {
  val <- gsub("[^0-9]", "", id)
  len <- nchar(val)
  actual_dv <- substr(val, len - 1, len)
  calc_dv <- carf_calc_dig(substr(val, 1, len - 2), verify = FALSE)
  actual_dv == calc_dv
}

#' Add separators to CARF lawsuits
#' 
#' @param id One or more lawsuit ids
#' 
#' @export
carf_build_id <- function(id) {
  val <- gsub("[^0-9]", "", id)
  if (all(nchar(val) %in% c(15, 17))) {
    mask <- "([0-9]{5})([0-9]{6})([0-9]{2}|[0-9]{4})([0-9]{2}$)"
    pattern <- "\\1.\\2/\\3-\\4"
    gsub(mask, pattern, val)
  } else {
    stop ("Length must be 15 or 17.")
  }
}
