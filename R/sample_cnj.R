#' Sample brazillian process codes
#'
#'
#'
#'@export
sample_cnj <- function(n, foros, anos, orgao, tr, first_dig = '0'){

  runif(n) %>%
    as.character %>%
    stringr::str_sub(3,8) %>%
    dplyr::data_frame() %>%
    dplyr::mutate(n_processo = abjutils::calc_dig(sprintf('%s%s%s%s%s%s',
                                                              first_dig,
                                                              .,
                                                              sample(rep(anos,2),n,replace = T),
                                                              orgao,
                                                              tr,
                                                              sample(rep(foros,2),n,replace = T)
    ), monta = T)) %>%
    dplyr::select(n_processo)
}
