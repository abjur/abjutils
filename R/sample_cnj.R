#' Sample brazilian lawsuits identification number
#' 
#' \code{sample_cnj} returns a data_frame containing a random sample of docket 
#' numbers distributed according to some regional and jurisdictional parameters.
#' The implementation supports both vector and scalar parameters, depending 
#' whether or not the function should uniformly sample from a scope of docket
#' numbers or one should define the parameters for each sample unit.
#' 
#' @param n a non negative integer giving the number of process codes to sample.
#' @param foros a string scalar (or vector) with 4 characters. Identifies the 
#'   juridical forum for the sampled codes.
#' @param anos a string scalar (or vector) with 4 characters. Identifies the 
#'   distribution years of the sampled codes.
#' @param orgao a string scalar (or vector) with 1 character. Identifies the 
#'   jurisdiction of the sampled codes.
#' @param tr a string scalar (or vector) with 2 characters. Identifies the court
#'   of the sampled codes.
#' @param first_dig the first digit of the process code. It's usually "0" or 
#'   "1". "0" as detault. The first digit will be sampled if first_dig = "".
#' @param sample_pars a logical scalar. Does the parameters define the 
#'   characteristics of the codes or should be sampled as well?
#' @param return_df Logical scalar. Should the function return a df? If FALSE 
#'   the function returns a vector.
#'   
#' @return A data_frame or a vector contaning a random sample of lawsuits ID's.
#'   
#' @examples
#' 
#' #sampling the parameters
#' sample_cnj(3, foros = "0000",
#' anos = "2015", orgao = 8, tr = 26,
#' first_dig = "0",sample_pars = TRUE, return_df = FALSE)
#' 
#' sample_cnj(10, foros = c("0000","0001"),
#' anos = c("2014","2015"), orgao = 8, tr = 26,
#' first_dig = "0",sample_pars = TRUE, return_df = FALSE)
#' 
#' #not sampling the parameters
#' 
#' sample_cnj(3, foros = c("0000","0001","0002"),
#' anos = c("2014","2015","2016"), orgao = rep(8,3), tr = rep(26,3),
#' first_dig = "0",sample_pars = FALSE, return_df = FALSE)
#' 
#' @export
sample_cnj <- function(n, foros, anos, orgao, tr, first_dig = '0', sample_pars = T,
                       return_df = T){

  # checks
  if(sample_pars){
    foros <- build_params_list(foros, n)
    anos <- build_params_list(anos, n)
    orgao <- build_params_list(orgao, n)
    tr <- build_params_list(tr, n)
  } else {
    
    lengths <- sapply(list(foros, anos, orgao, tr), length)
    
    max_length <- max(lengths)
    
    logical_test <- max_length != min(lengths)
    
    if(logical_test){
      stop("When sample_pars is FALSE, sample_cnj expects foros, anos, orgaos and tr with same length.")
    } else {
      if(max_length != 1 & (max_length != n)){
        stop("When sample_pars is FALSE, sample_cnj expects parameters lengths to be equal to n.")
      }
    }
  }
  #end of checks
  
  serial_size <- ifelse(first_dig == "", 9, 8)
  
  #main code
  ret <- runif(n) %>%
    as.character %>%
    stringr::str_sub(3,serial_size) %>%
    dplyr::data_frame() %>%
    stats::setNames("serial") %>% 
    dplyr::mutate(no_cd_code = sprintf("%s%s%s%s%s%s", first_dig,
                  serial, anos, orgao, tr, foros),
      n_processo = calc_dig(no_cd_code, build = T)) %>%
    dplyr::select(n_processo)
  
  #df or vec
  if(return_df){
    return(ret)
  } else {
    return(with(ret, n_processo))
  }
}

build_params_list <- function(x, n){
  if (length(x) > 1){
    sample(as.character(x), n, replace = T)
  } else {
    rep(x, n)
  }
}