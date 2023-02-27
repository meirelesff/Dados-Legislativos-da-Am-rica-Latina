# ---
# 0 COLETA DADOS DE PROPOSICOES LEGISLATIVAS DO BRASIL, ARGENTINA E CHILE
# ---


# Pacotes
library(tidyverse)
library(chamberBR)
library(rvest)
library(httr)
library(here)


# Cria dir para salvar os dados
if(!dir.exists(here("dados"))) dir.create(here("dados"))


# Coleta os dados
votos_br <- 2001:2022 %>%
  map_df(chamberBR::pega_votos, orientacao = FALSE)

votacoes_br <- 2001:2022 %>%
  map_df(pega_votacoes)

pls_brasil <- pega_proposicoes(2000:2022)
pls_argentina <- read_csv("https://datos.hcdn.gob.ar/dataset/839441fc-1b5c-45b8-82c9-8b0f18ac7c9b/resource/22b2d52c-7a0e-426b-ac0a-a3326c388ba6/download/proyectos-parlamentarios1.6.csv")
pls_uruguai <- "https://parlamento.gub.uy/documentosyleyes/proyectos-entrados/csv?Ast_FechaDeEntradaAlCuerpo%5Bmin%5D%5Bdate%5D=01-01-2000&Ast_FechaDeEntradaAlCuerpo%5Bmax%5D%5Bdate%5D=22-02-2022&Cpo_Codigo=D" %>%
  read_csv()

votacoes_argentina <- read_csv("https://datos.hcdn.gob.ar/dataset/2e08ab84-09f4-4aac-86b3-9573ca9810db/resource/28bdc184-d8e3-4d50-b5b5-e2151f902ac7/download/actas-datos-generales-2.4.csv")
votos_argentina <- read_csv("https://datos.hcdn.gob.ar/dataset/2e08ab84-09f4-4aac-86b3-9573ca9810db/resource/262cc543-3186-401b-b35e-dcdb2635976d/download/detalle-actas-datos-generales-2.4.csv")

# Salva os dados
save(pls_brasil, pls_argentina, pls_uruguai, votos_argentina, votacoes_argentina, file = here("dados", "bills_argentina_brasil_urugrai.Rda"))
save(votos_br, votacoes_br, file = here("dados", "votos_br.Rda"))
