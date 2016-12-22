#' Sample brazillian process codes.
#'
#' \code{sample_cnj} returns a data_frame containing a random sample of
#' process codes sampled according to some regional and jurisdictional 
#' parameters. The actual implementation supports both vector or scalar parameters,
#' depending wheter or not one whants to sample from a determined scope of
#' process codes or whants to define every single set of parameters.
#'
#' @param n a non negative integer giving the number of process codes to sample.
#' @param foros a string scalar (or vector) with 4 characters. Identifies the place
#' where the sampled processes need to be distributed.
#' @param anos a string scalar (or vector) with 4 characters. Identifies the 
#' distribution years of the sampled codes.
#' @param orgao a string scalar (or vector) with 1 character. Identifies the jurisdiction
#' of the sampled codes.
#' @param tr a string scalar (or vector) with 2 character. Identifies the court of 
#' the sampled codes.
#' @param first_dig the first digit of the process code. It's usually "0" or "1". 
#' "0" as detault. The first digit will be sampled if first_dig = "".
#' @param sample_pars Logical scalar.
#' Does the parameters define the characteristics of the codes or should be sampled
#' as well?
#' @param return_df Logical scalar. Should the function return a df? If FALSE the
#' function returns a vector.
#'  
#' @return A data_frame or a vector contaning a random sample of processes of size n.
#' 
#' @export
sample_cnj <- function(n, foros, anos, orgao, tr, first_dig = '0', sample_pars = T,
                       return_df = T){

  # checks
  if(sample_pars){
    foros <- sample(foros, n, replace = T)
    anos <- sample(anos, n, replace = T)
    orgao <- sample(orgao, n, replace = T)
    tr <- sample(tr, n, replace = T)
  } else {
    
    lengths <- sapply(c(foros, anos, orgao, tr), length)
    
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
  #end of
  
  serial_size <- ifelse(first_dig == "", 9, 8)
  
  runif(n) %>%
    as.character %>%
    stringr::str_sub(3,serial_size) %>%
    dplyr::data_frame() %>%
    stats::setNames("serial") %>% 
    dplyr::mutate(n_processo = calc_dig(sprintf('%s%s%s%s%s%s',
                                                              first_dig,
                                                              serial,
                                                              anos,
                                                              orgao,
                                                              tr,
                                                              foro
    ), build = T)) %>%
    dplyr::select(n_processo)
}
