#' Funcao.
#' @export
pega_mov_tjsp <- function(p) {
  p <- gsub('[^0-9]','',as.character(p))
  
  dados_url <- list(
    paginaConsulta              = '1',
    localPesquisa.cdLocal       = '-1',
    cbPesquisa                  = 'NUMPROC',
    tipoNuProcesso              = 'UNIFICADO',
    numeroDigitoAnoUnificado    = str_sub(p,end=15),
    foroNumeroUnificado         = str_sub(p,start=22),
    dePesquisaNuUnificado       = p,
    dePesquisa                  = ''
  )
  
  url <- 'http://esaj.tjsp.jus.br/cpo/pg/search.do'
  url2 <- paste(url, paste0(paste(names(dados_url), unlist(dados_url), sep = '='), collapse = '&'), sep = '?')
  r <- GET(url2)
  k <- T
  while(r$status_code != 200) {
    if(k) {
      cat('\nesperando...')
    } else {
      cat('...')
    }
    Sys.sleep(2)
    r <- GET(url2)
    k <- F
  }
  if(!k) cat('\n')
  html <- htmlParse(content(r, as='text'),encoding='UTF-8')
  tabela <- getNodeSet(html, "//table[@id='tabelaTodasMovimentacoes']//tr") 
  mov <- data.frame(str_trim(str_split_fixed(str_trim(gsub('[\n\t\r]', '', sapply(tabela, xmlValue))), ' ', 2)), stringsAsFactors = F)
  names(mov) <- c('data', 'movimentacao')  
  if(nrow(mov)>0) {
    mov$processo <- p
    return(mov)  
  }
}
