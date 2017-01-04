#' Calculate check digits for brazilian lawsuits identification number
#' 
#' \code{calc_dig} returns the check digit of a docker numbers in the format 
#' unified by the brazillian National Council of Justice.
#' @param num Ordered digits of the docket number (including 0's) excluding the 
#'   check digit.
#' @param build Logical scalar. Should the function return the complete docket
#'   number or only the check digits?
#'   
#' @return Returns the check digits or the complete identification number.
#'   
#' @examples
#' 
#' calc_dig("001040620018260004", build = TRUE)
#' calc_dig("001040620018260004", build = FALSE)
#' 
#' #will fail
#' \dontrun{
#' calc_dig("00104062001826000", build = TRUE)
#' }
#' @export
calc_dig <- function(num, build = FALSE) {
  
  lengths <- stringr::str_length(num)
  
  if(max(lengths) != 18 | min(lengths) != 18){
    stop("Process codes without check digits should have 18 numerical digits.")
  }
  
  NNNNNNN <- substr(num,  1L,  7L)
  AAAA <-    substr(num,  8L, 11L)
  JTR <-     substr(num, 12L, 14L)
  OOOO <-    substr(num, 15L, 18L)
  n1 <- sprintf('%02d', as.numeric(NNNNNNN) %% 97)
  n2 <- sprintf('%02d', as.numeric(sprintf('%s%s%s', n1, AAAA, JTR)) %% 97)
  n3 <- sprintf('%02d', 98 - ((as.numeric(sprintf('%s%s', n2, OOOO)) * 100) %% 97))
  dig <- n3
  if(build) {
    return(sprintf('%s%s%s', substr(num, 1, 7), dig, substr(num, 8, 18)))
  }
  return(dig)
}

#' Validate check digits for brazilian lawsuits identification number
#' 
#' \code{check_dig} verifies if a check digit is correct.
#' 
#' @param num String scalar containing the complete docket number.
#' @return Logical scalar indicating whether or not the check digit is well 
#'   calculated.
#'   
#' @examples
#' 
#' check_dig("0005268-75.2013.8.26.0100")
#' 
#' \dontrun{
#' check_dig("0005268-75.2013.8.26.100", build = TRUE)
#' }
#' @export
check_dig <- function(num) {
  
  num <- stringr::str_replace_all(num, "[.-]","")
  
  if(stringr::str_length(num) != 20){
    stop("Complete docket numbers should have 20 numerical digits.")
  }
  
  num_no_dig <- stringr::str_c(substr(num,1L,7L),substr(num, 10L, 20L))

  num_with_dig <- calc_dig(num_no_dig, build = T)
  
  return(identical(num_with_dig, num))
}