abjutils [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/abjur/abjutils?branch=master&svg=true)](https://ci.appveyor.com/project/abjur/abjutils/branch/master) [![Travis-CI Build Status](https://travis-ci.org/abjur/abjutils.svg?branch=master)](https://travis-ci.org/abjur/abjutils)
========

Funções úteis para a ABJ.

## Relatórios

- `tabela` tabela de contingência em formato long, com frequências absolutas e relativas para uma resposta.

## Utilidades

- `rm_accent` para tirar os acentos de um texto (vetorizado). Foi implementada buscando compatibilidade tanto com Windows quanto com sistemas operacionais baseados em Unix.
- `lsos` lista objetos do workspace de forma elegante.
- `dvec` vetoriza *web scrapers*.

## Manipulação de números de processo

- `calc_dig` calcula o digito verificador de um número de processo no formato pelo Conselho Nacional de Justiça (CNJ) na [resolução 65/08](http://www.cnj.jus.br/images/stories/docs_cnj/resolucao/rescnj_65.pdf).
- `check_dig` verifica se o digito verificador de um número de processo foi calculado corretamente.
- `sample_cnj` produz números de processo aleatoriamente.
