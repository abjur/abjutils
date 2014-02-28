dados_url <- list(
  paginaConsulta='1',
  localPesquisa.cdLocal='-1',
  cbPesquisa='NUMPROC',
  tipoNuProcesso='UNIFICADO',
  numeroDigitoAnoUnificado='',
  foroNumeroUnificado='',
  dePesquisaNuUnificado='',
  dePesquisa=''
)

#' Funcao.
#' @export
pega_metadados_tjsp <- function(p) {
  p <- gsub('[^0-9]','',as.character(p))
  dados_url[['numeriDigitoAnoUnificado']] <- str_sub(p,end=15)
  dados_url[['foroNumeroUnificado']] <- str_sub(p,start=22)
  dados_url[['dePesquisaNuUnificado']] <- p
  url <- 'http://esaj.tjsp.jus.br/cpo/pg/search.do'
  url2 <- paste(url, paste0(paste(names(dados_url),unlist(dados_url),sep='='),collapse='&'),sep='?')
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
  
  vet_dados <- sapply(getNodeSet(html, "//table[@id != 'secaoFormConsulta' and (@class='secaoFormBody' or @id='tablePartesPrincipais')]//tr//td"),xmlValue)
  vet_dados <- str_trim(gsub(' +',' ',gsub('[\n\t\r]','',vet_dados)))
  vet_dados <- vet_dados[!duplicated(vet_dados,incomparables="")]
  # print(vet_dados)
  df_dados <- data.frame(key=str_trim(gsub(' +',' ',gsub('[\n\t\r]','',vet_dados[1:length(vet_dados) %% 2 == 1]))),
                         value=str_trim(gsub(' +',' ',gsub('[\n\t\r]','',vet_dados[1:length(vet_dados) %% 2 == 0]))))
  df_dados$n_processo <- p
  return(df_dados[,c(3,1,2)])
}