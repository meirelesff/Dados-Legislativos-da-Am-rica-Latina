# ---
# 2 ESTATISTICAS DESCRITIVAS
# ---


# Pacotes
library(kableExtra)
library(tidyverse)
library(here)


# Carrega os dados
load(here("dados", "bills_argentina_brasil_urugrai.Rda"))
load(here("dados", "votos_br.Rda"))

load(here("dados", "votos_mexico.rda"))
votos_mexico <- votos

load(here("dados", "proposicoes_colombia.Rda"))
load(here("dados", "proposicoes_paraguai.Rda"))
props_paraguai <- props

load(here("dados", "proposicoes_rep_dominicana.Rda"))
prop_dom <- props


# Tabela de proposicoes por pais e periodo

argentina <- pls_argentina %>%
  filter(str_detect(proyecto_tipo, "LEY")) %>%
  mutate(ano = lubridate::year(publicacion_fecha)) %>%
  summarise(pais = "Argentina", 
            inicio = min(ano), fim = max(ano),
            media = n() / length(unique(ano)),
            pls = n())
            
uruguai <- pls_uruguai %>%
  mutate(ano = lubridate::year(lubridate::dmy(Fecha))) %>%
  summarise(pais = "Uruguai", 
            inicio = min(ano), fim = max(ano),
            media = n() / length(unique(ano)),
            pls = n())

brasil <- pls_brasil %>%
  mutate(ano = lubridate::year(data_apresentacao)) %>%
  summarise(pais = "Brasil", 
            inicio = min(ano), fim = max(ano),
            media = n() / length(unique(ano)),
            pls = n())

dominicana <- prop_dom %>%
  filter(camaraInicio == "Cámara de Diputados") %>%
  filter(tipo == "Proyecto de Ley") %>%
  mutate(ano = lubridate::year(fechaDeposito)) %>%
  summarise(pais = "Rep. Dominicana", 
            inicio = min(ano), fim = max(ano),
            media = n() / length(unique(ano)),
            pls = n())

paraguai <- props_paraguai %>%
  filter(tipoProyecto == "PROYECTO DE LEY") %>%
  mutate(ano = lubridate::year(lubridate::dmy(fechaIngresoExpediente))) %>%
  summarise(pais = "Paraguai", 
            inicio = min(ano), fim = max(ano),
            media = n() / length(unique(ano)),
            pls = n())

colombia <- leis_col %>%
  filter(tipo == "Proyecto de Ley") %>%
  mutate(ano = lubridate::year(data)) %>%
  summarise(pais = "Colômbia", 
            inicio = min(ano, na.rm = T), fim = max(ano, na.rm = T),
            media = n() / length(unique(ano)),
            pls = n())

bind_rows(argentina, brasil, colombia, dominicana, paraguai, uruguai) %>%
  mutate(media = round(media, 1)) %>%
  set_names(c("País", "Início", "Fim", "PLs (média/ano)", "Total")) %>%
  kable("latex", caption = "Distribuição de projetos de lei na amostra", booktabs = T,
        label = "pls", linesep = "") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable(here("outputs", "tab_pls.tex"))

rm(list = ls())


# Votacoes
load(here("outputs", "apres.Rda"))

bind_rows(apre_argentina, apre_br, apre_mexico) %>%
  group_by(pais) %>%
  summarise(n_votacoes = sum(n_votos[dims == 2]),
            media = round(n_votacoes / length(unique(legislatura)), 1),
            legislaturas = length(unique(legislatura))) %>%
  
  set_names(c("País", "Votações", "Média/ano", "Legislaturas")) %>%
  kable("latex", caption = "Distribuição de projetos de votações nominais na amostra", booktabs = T,
        label = "votacoes", linesep = "") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable(here("outputs", "tab_votacoes.tex"))


# Entradas
bind_rows(argentina, brasil, colombia, dominicana, paraguai, uruguai) %>%
  summarise(total = sum(pls))

bind_rows(apre_argentina, apre_br, apre_mexico) %>%
  group_by(pais) %>%
  summarise(n_votacoes = sum(n_votos[dims == 2])) %>%
  summarise(total = sum(n_votacoes))



