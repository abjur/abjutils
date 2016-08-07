devtools::install_github('abjur/tjsp')

unloadNamespace('tjsp')
library(magrittr)
library(tjsp)

path0 <- '/home/storage/abj/raw/TJSP/cjsg'
dir.create(path0)

sec <- list_secoes_2inst() %>% 
  dplyr::filter(stringr::str_detect(secao, '[Cc]rim'),
                stringr::str_detect(pai, 'CRIM')) %>% 
  with(cod)

d_result <- list()
unlink('/home/storage/abj/raw/TJSP/cjsg/cjsg_mes04/', recursive = TRUE)
for(i in 4) {
  session <- cjsg_session()
  parms <- session %>% 
    cjsg_parms(secoes = sec, 
               data_inicial = sprintf('2015-%02d-01', i), 
               data_final = lubridate::ymd(sprintf('2015-%02d-01', (i + 1) %% 12)) - 1)
  session %>% cjsg_npags(parms) %>% print()
  d_result[[i]] <- session %>% 
    cjsg(parms, path = sprintf('%s/cjsg_mes%02d', path0, i), max_pag = Inf)
}

arqs <- dir(path0, full.names = TRUE) %>% 
  lapply(dir, full.names = TRUE)

lapply(4, function(i) {
  d_cjsg <- arqs[[i]] %>% parse_cjsg()
  saveRDS(d_cjsg, sprintf('%s/d_cjsg_%02d.rds', path0, i))
  mtcars
})

saveRDS(d_cjsg, sprintf('%s/d_cjsg.rds', path0))

rds <- dir(path0, full.names = TRUE) %>% 
  `[`(tools::file_ext(basename(.)) == 'rds' & grepl('[0-9]', basename(.)))

d_cjsg <- lapply(rds, readRDS) %>% 
  dplyr::bind_rows()
saveRDS(d_cjsg, sprintf('%s/d_cjsg.rds', path0))







