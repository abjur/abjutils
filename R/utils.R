#' Vetorizando scrapers
#'
#' Vetoriza um scraper (função) para um vetor de itens
#'
#' @param fun função a ser aplicada em cada arquivo.
#' @param itens character vector dos caminhos de arquivos a serem transformados.
#' @param ... outros parâmetros a serem passados para \code{fun}
#' @param verbose se \code{TRUE} (default), mostra o item com probabilidade p.
#' @param p probabilidade de imprimir mensagem.
#' 
#' @export
dvec <- function(fun, itens, ..., verbose = TRUE, p = .05) {
  f <- dplyr::failwith(tibble::data_frame(result = 'erro'), fun)
  tibble::data_frame(item = itens) %>%
    dplyr::distinct(item) %>%
    dplyr::group_by(item) %>%
    dplyr::do({
      if (runif(1) < p && verbose) print(.$item)
      d <- f(.$item, ...)
      if (!tibble::has_name(d, 'result')) d$result <- 'OK'
      d
    }) %>%
    dplyr::ungroup()
}

#' Remove acentos.
#' 
#' Remove os acentos da string.
#' 
#' @param x texto ou vetor de textos.
#' 
#' @export
rm_accent <- function(x) {
  if (.Platform$OS.type == 'unix') {
    gsub("`", "", iconv(x, to = "ASCII//TRANSLIT"))
  } else {
    gsub("`", "", iconv(x, from = 'latin1', to="ASCII//TRANSLIT"))
  }
}

.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

#' Lista objetos.
#' 
#' @param ... outros parâmetros passados para \code{.ls.objects}.
#' @param n apenas os n com maior tamanho.
#' 
#' @export
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

#' Criar repositório e template de R-package
#' 
#' Só funciona se tiver um \code{Sys.getenv("GITHUB_PAT")}.
#' 
#' @param repo nome do repo.
#' @param org nome da organização.
#' @param path caminho do repo.
#' @param descr descrição do pacote. Se nulo, tentará pegar de \code{Sys.getenv('REPO_DESC')}.
#' 
#' @export
github_template <- function(repo, org = '', path = '.', descr = NULL) {
  if (is.null(descr)) descr <- list('Maintainer' = Sys.getenv('REPO_DESC'))
  auth_token <- devtools::github_pat(quiet = TRUE)
  if (org != '') {
    url <- sprintf('https://api.github.com/orgs/%s/repos?access_token=%s', org, auth_token)
  } else {
    org <- 'jtrecenti'
    url <- sprintf('https://api.github.com/user/repos?access_token=%s', auth_token)
  }
  httr::POST(url, body = list(
    name = repo, auto_init = TRUE, license_template = 'mit'
  ), encode = 'json')
  system(sprintf('git clone https://github.com/%s/%s %s/%s', org, repo, path, repo))
  if (!file.exists(path)) dir.create(path)
  path_repo <- sprintf('%s/%s', path, repo)
  devtools::setup(path_repo, descr)
  devtools::use_data_raw(path_repo)
}
