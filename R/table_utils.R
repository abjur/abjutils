prettify_number <- function(d, number = T, percent = T,  ...){
  
  if(percent){
    query <- '{fmt_p(x/sum(x))}'
  } 
  
  if(number) {
    query <- sprintf('{fmt(x)} (%s)', glue::glue(query))
  }
  
  d %>% 
    dplyr::select(...) %>% 
    dplyr::mutate_if(is.numeric, .funs = function(x){glue::glue(query)}) %>% 
    dplyr::as_data_frame()
}

fmt <- function(x) {
  format(x, big.mark = '.', small.mark = ',',
         decimal.mark = ',',
         scientific = FALSE, trim = TRUE)
}

fmt_p <- function(x) {
  if (length(x) == 0) return(character())
  x <- plyr::round_any(x, precision(x) / 100)
  x <- fmt(x * 100)
  paste0(x, "%")
}