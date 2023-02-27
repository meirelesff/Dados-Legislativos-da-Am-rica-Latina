# Dados Legislativos da América Latina

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


Este repositório contém os códigos para reproduzir e atualizar a coleta de dados de proposições legislativas e votações nominais de países da América Latina que disponibilizam repositórios de dados abertos em suas câmaras baixas ou unicamerais. Códigos e dados aqui contidos são fruto de pesquisa de Pós-Doutorado, vinculado ao Cebrap, financiada pela Fapesp sob o processo nº `2021/01393-2`.


### Amostra

Atualmente, a base de dados aqui contida engloba o seguintes países:

| País           | Repositório             | Votações   | Proposições | Coletado   |
|----------------|------------------------|------------|-------------|------------|
| Argentina| Encontrado             | ✔          | ✔           | ✔          |
| Bolívia        | Não encontrado         | --         | --          | --         |
| Brasil   | Encontrado             | ✔          | ✔           | ✔          |
| Chile    | Inativo                | ✔          | ✔           | ❌          |
| Colômbia | Encontrado             | ❌         | ✔           | ✔          |
| Costa Rica| Incompleto             | --         | --          | --         |
| El Salvador    | Não encontrado         | --         | --          | --         |
| Equador  | Incompleto             | ❌         | ❌           | ❌          |
| Guatemala      | Não encontrado         | --         | --          | --         |
| Honduras       | Não encontrado         | --         | --          | --         |
| México   | Incompleto             | ✔          | ❌           | ✔          |
| Nicarágua      | Não encontrado         | --         | --          | --         |
| Panamá         | Não encontrado         |            |             |            |
| Paraguai | Encontrado             | ✔          | ❌           | ✔          |
| Peru           | Não encontrado         | --         | --          | --         |
| Rep. Dominicana | Incompleto        | ❌         | ✔           | ✔          |
| Uruguai | Encontrado             | ❌         | ✔           | ✔          |


Para cada um dos casos e tipo de dado, os códigos contidos na pasta `scripts` extraem, processam e salvam dados em formato `.Rda` na pasta `dados` com informações legislativas já processadas.


## Replicação

Para replicar a coleta de dados, basta abrir e rodar os `scripts` na pasta de mesmo nome. Os pacotes mais importantes no processo de coleta e limpeza dos são:

```r
library(tidyverse)
library(rvest)
library(httr)
library(here)
```

Para facilitar a replicação, dependências são geridas com [`renv`](https://rstudio.github.io/renv/articles/renv.html), que permite a restauração de versões apropriadas. Para tanto, basta rodar:

```r
if (!require("renv")) install.packages("renv")
renv::hydrate()
```











