abjutils [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/abjur/abjutils?branch=master&svg=true)](https://ci.appveyor.com/project/abjur/abjutils/branch/master) [![Travis-CI Build Status](https://travis-ci.org/abjur/abjutils.svg?branch=master)](https://travis-ci.org/abjur/abjutils)
========

Funções úteis para a ABJ.

## Relatórios

- `tabela` tabela de contingência em formato long, com frequências absolutas e relativas para uma resposta.
- `ggbarplot` gráfico de barras para uma resposta categórica e até duas explicativas.
- `ggboxplot` gráfico boxplot para uma resposta contínua e até duas explicativas.

## Utilidades

- `rm_accent` ou `desacentuar` para tirar os acentos de um texto (vetorizado).
- `lsos` lista objeto de forma elegante.

## Bases de dados

- `br_uf_map`: para plotar o mapa do Brasil com ufs (fortified map).
- `cadmun`: infos básicas dos municípios, incluindo lat e lon.
- `dados_aj`: infos básicas de órgãos da justiça, obtido por raspagem do Atlas do Acesso à Justiça.
- `dados_muni`: latitude e longitude dos municípios brasileiros.
- `pnud_muni`: dados do PNUD a nível município, com dicionário em `pnud_siglas`.
- `pnud_uf`: dados do PNUD a nível UF, com dicionário em `pnud_siglas`.
- `pnud_siglas`: dicionário das variáveis das bases do PNUD.
