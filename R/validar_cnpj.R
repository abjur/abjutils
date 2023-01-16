#' Valida um CNPJ
#' 
#' Recebe um CNPJ e retorna TRUE ou FALSE. Nao vetorizado.
#' 
#' @param cnpj string com o CNPJ a ser validado
#' 
#' @return Retorna TRUE ou FALSE
#' @export
validar_cnpj <- function(cnpj) {
  cnpj <- gsub("[^[:digit:]]", "", cnpj)
  if(cnpj == '') return(FALSE)
  
  if (nchar(cnpj) != 14)
    return(FALSE)
  
  # Elimina CNPJs invalidos conhecidos
  invalidos <- c(
    "00000000000000", "11111111111111", "22222222222222", "33333333333333",
    "44444444444444", "55555555555555", "66666666666666", "77777777777777", 
    "88888888888888", "99999999999999"
  )
  if(cnpj %in% invalidos) return(FALSE)
  
  # Valida DVs
  tamanho <- nchar(cnpj) - 2
  numeros <- substr(cnpj, 1, tamanho)
  digitos <- substr(cnpj, tamanho + 1, nchar(cnpj))
  soma <- 0
  pos <- tamanho - 7
  for (i in tamanho:1) {
    soma <- soma + as.numeric(substr(numeros, tamanho - i + 1, tamanho - i + 1)) * pos
    pos <- pos - 1
    if (pos < 2)
      pos <- 9
  }
  resultado <- ifelse(soma %% 11 < 2, 0, 11 - soma %% 11)
  if (resultado != as.numeric(substr(digitos, 1, 1)))
    return(FALSE)
  
  tamanho <- tamanho + 1
  numeros <- substr(cnpj, 1, tamanho)
  soma <- 0
  pos <- tamanho - 7
  for (i in tamanho:1) {
    soma <- soma + as.numeric(substr(numeros, tamanho - i + 1, tamanho - i + 1)) * pos
    pos <- pos - 1
    if (pos < 2)
      pos <- 9
  }
  resultado <- ifelse(soma %% 11 < 2, 0, 11 - soma %% 11)
  if (resultado != as.numeric(substr(digitos, 2, 2)))
    return(FALSE)          
  return(TRUE)  
}
