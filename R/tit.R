#' @title Calculate check digit for TIT
#' 
#' @description Returns the check digit of a TIT number or full number with
#' the check digit.
#' 
#' @param id Lawsuit number (including trailling zeros), excluding
#' the check digit.
#' @param build Whether or not the function return the complete number
#' (or only the check digits)?
#'
#' @return The check digits or the complete identification number
#'   
#' @examples {
#' tit_calc_dig("3107557", build = TRUE)
#' }
#' 
#' @export
tit_calc_dig <- function(id, build = FALSE) {
  
  val <- gsub("[^0-9]", "", id)
  
  calc_dv_one <- function(val) {
    nums <- as.numeric(unlist(strsplit(val, "")))
    len <- length(nums)
    total <- sum(seq(len + 1, 2) * nums)
    (total %% 11) %% 10
  }
  
  dv <- purrr::map_chr(val, calc_dv_one)
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
#' tit_check_dig("3107557-5")
#' tit_check_dig(c("30385040", "10008596-9"))
#' }
#' @export
tit_check_dig <- function(id) {
  val <- gsub("[^0-9]", "", id)
  len <- nchar(val)
  actual_dv <- substr(val, len, len)
  calc_dv <- tit_calc_dig(substr(val, 1, len - 1))
  actual_dv == calc_dv
}

#' Add separators to TIT lawsuits
#' 
#' @param id One or more lawsuit ids
#' 
#' @export
tit_build_num <- function(id) {
  gsub("([0-9]+)([0-9]$)", "\\1-\\2", id)
}