# ---
# 1 GRAFICO COM A DISTRIBUICAO DOS DADOS
# ---


# Pacotes
library(tidyverse)
library(here)
library(oc)

source(here("_funcoes.R"))


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


### PROPOSICOES ----------------------------------------------------------------


# Distribuicao de proposicoes por ano
argentina <- pls_argentina %>%
  filter(str_detect(proyecto_tipo, "LEY")) %>%
  mutate(ano = lubridate::year(publicacion_fecha)) %>%
  group_by(ano) %>%
  summarise(total = n()) %>%
  mutate(pais = "Argentina")

uruguai <- pls_uruguai %>%
  mutate(ano = lubridate::year(lubridate::dmy(Fecha))) %>%
  group_by(ano) %>%
  summarise(total = n()) %>%
  mutate(pais = "Uruguai")

brasil <- pls_brasil %>%
  mutate(ano = lubridate::year(data_apresentacao)) %>%
  group_by(ano) %>%
  summarise(total = n()) %>%
  mutate(pais = "Brasil")

dominicana <- prop_dom %>%
  filter(camaraInicio == "Cámara de Diputados") %>%
  filter(tipo == "Proyecto de Ley") %>%
  mutate(ano = lubridate::year(fechaDeposito)) %>%
  group_by(ano) %>%
  summarise(total = n()) %>%
  mutate(pais = "Rep. Dominicana")

paraguai <- props_paraguai %>%
  filter(tipoProyecto == "PROYECTO DE LEY") %>%
  mutate(ano = lubridate::year(lubridate::dmy(fechaIngresoExpediente))) %>%
  group_by(ano) %>%
  summarise(total = n()) %>%
  mutate(pais = "Paraguai")

colombia <- leis_col %>%
  filter(tipo == "Proyecto de Ley") %>%
  mutate(ano = lubridate::year(data)) %>%
  group_by(ano) %>%
  summarise(total = n()) %>%
  mutate(pais = "Colômbia")
  
  
bind_rows(brasil, uruguai, argentina, dominicana, paraguai, colombia) %>% 
  filter(ano < 2022 & ano > 1990) %>%
  ggplot(aes(x = ano, y = total)) +
  geom_line(color = purple, size = 0.72) +
  facet_wrap(~ pais, scales = "free_y", ncol = 2) +
  tema() +
  labs(y = "Projetos de Lei apresentados\nna câmara baixa",
       x = NULL) +
  ylim(0, NA)

ggsave(filename = here("outputs", "projetos_lei.pdf"), width = 5.5, height = 5)


### VOTACOES NOMINAIS ----------------------------------------------------------


# Argentina
argentina <- votacoes_argentina %>%
  mutate(ano = lubridate::year(fecha)) %>%
  group_by(ano) %>%
  summarise(total = n()) %>%
  mutate(pais = "Argentina")

# Brasil
brasil <- votacoes_br %>%
  filter((votos_sim + votos_nao) > 0) %>%
  filter(sigla_orgao == "PLEN") %>%
  mutate(ano = lubridate::year(data)) %>%
  group_by(ano) %>%
  summarise(total = n()) %>%
  mutate(pais = "Brasil") 

bind_rows(brasil, argentina) %>% 
  filter(ano > 2003) %>%
  ggplot(aes(x = ano, y = total)) +
  geom_line(color = purple, size = 0.72) +
  facet_wrap(~ pais, scales = "free_y", ncol = 2) +
  tema() +
  labs(y = "Votações nominais\nna câmara baixa",
       x = NULL) +
  ylim(0, NA)

ggsave(filename = here("outputs", "votacoes_ano.pdf"), width = 5.5, height = 5)


### OC -------------------------------------------------------------------------


# Funcao para rodar OC e extrair outputs uteis
roda_oc <- function(rc){
  
  dim1 <- rc %>%
    oc(polarity = c(1), dims = 1)
  
  dim2 <- rc %>%
    oc(polarity = c(1, 1), dims = 2)
  
  apre <- tibble(dims = c(1, 2), apre = c(dim1$fits[2], dim2$fits[2]),
                 predito = c(dim1$fits[1], dim2$fits[1]),
                 n_votos = c(nrow(dim1$rollcalls), nrow(dim2$rollcalls)))
  
  estimates <- dim2$legislators %>%
    as_tibble(rownames = "deps") %>%
    select(deps, coord1D)
  
  list(apre = apre, estimates = estimates)
} 


# Funcao para extrair nome das legislaturas
extrai_legislatura <- function(res){
  
   str_match(res$estimates$deps[1], "([^-]+)$")[,1]
}


# Argentina
votos_argentina <- votos_argentina %>% 
  left_join(select(votacoes_argentina, acta_id, nroperiodo)) %>% 
  mutate(voto = case_when(voto == "AFIRMATIVO" ~ 1L, 
                          voto == "NEGATIVO" ~ 0L, 
                          TRUE ~ 0L)) %>%
  mutate(deputado_id = paste0(diputado_nombre, "-", bloque, "-", nroperiodo)) %>% 
  group_split(nroperiodo) %>%
  map(~ congressbr::vote_to_rollcall(.x$voto, 
                                     .x$deputado_id, 
                                     .x$acta_id))


# Brasil
ids <- votacoes_br %>%
  mutate(legislatura = case_when(
    
    data > as.Date("1999-01-01") & data < as.Date("2002-12-31") ~ 51,
    data > as.Date("2003-01-01") & data < as.Date("2006-12-31") ~ 52,
    data > as.Date("2007-01-01") & data < as.Date("2010-12-31") ~ 53,
    data > as.Date("2011-01-01") & data < as.Date("2014-12-31") ~ 54,
    data > as.Date("2015-01-01") & data < as.Date("2018-12-31") ~ 55,
    data > as.Date("2019-01-01") & data < as.Date("2022-12-31") ~ 56
    
  )) %>%
  select(id, legislatura)

votos_br <- votos_br %>%
  left_join(ids, by = c("id_votacao" = "id")) %>%
  mutate(voto = case_when(voto == "Sim" ~ 1L, 
                          voto == "Não" ~ 0L, 
                          TRUE ~ 0L)) %>%
  mutate(deputado_id = paste0(deputado_nome, "-", deputado_sigla_partido, "-", legislatura)) %>% 
  group_split(legislatura) %>%
  map(~ congressbr::vote_to_rollcall(.x$voto, 
                                     .x$deputado_id, 
                                     .x$id_votacao))


# Mexico
votos_mexico <- votos_mexico %>%
  unnest(votos) %>%
  mutate(id_voto = str_match(body, "evento=(\\d+)&")[, 2]) %>%
  mutate(legislatura = str_match(referer, "\\/(\\d+)\\/")[, 2]) %>%
  mutate(id_votacao = paste0(id_voto, "-", legislatura)) %>%
  
  mutate(voto = case_when(tipo == "Favor" ~ 1L, 
                          tipo == "Contra" ~ 0L, 
                          TRUE ~ 0L)) %>%
  mutate(deputado_id = paste0(deps, "-", partido, "-", legislatura)) %>%
  group_split(legislatura) %>%
  map(~ congressbr::vote_to_rollcall(.x$voto, 
                                     .x$deputado_id, 
                                     .x$id_votacao))


# Roda o OC
argentina <- votos_argentina %>%
  map(roda_oc)

brasil <- votos_br[-1] %>%
  map(roda_oc)

mexico <- votos_mexico %>%
  map(roda_oc)

save(argentina, brasil, mexico, file = here("dados", "oc.Rda"))


# Cria uma base de resumo
apre_br <- brasil %>%
  map_df(~ .x$apre %>%
        mutate(legislatura = extrai_legislatura(.x)) %>%
        mutate(pais = "Brasil"))

apre_argentina <- argentina %>%
  map_df(~ .x$apre %>%
           mutate(legislatura = extrai_legislatura(.x)) %>%
           mutate(pais = "Argentina"))

apre_mexico <- mexico %>%
  map_df(~ .x$apre %>%
           mutate(legislatura = extrai_legislatura(.x)) %>%
           mutate(pais = "México"))

save(apre_br, apre_argentina, apre_mexico, file = here("outputs", "apres.Rda"))

# Grafico de APRE
bind_rows(apre_br, apre_argentina, apre_mexico) %>%
  
  ggplot(aes(x = as.numeric(legislatura), y = apre, color = as.factor(dims))) +
  geom_line() +
  geom_point() +
  
  facet_wrap(~ pais, scales = "free") +
  tema() +
  scale_color_manual(values = paleta) +
  labs(y = "APRE\nna câmara baixa",
       x = "Legislatura", color = "Dimensões") +
  ylim(0, 1)

ggsave(filename = here("outputs", "apre_oc.pdf"), width = 6, height = 4)


# Grafico de polarizacao

brasil %>%
  map_df(~ .x$estimates %>%
           mutate(legislatura = extrai_legislatura(.x)) %>%
           mutate(pais = "Brasil") %>%
           mutate(partido = str_match(deps, "-([^-]+)-")[,2])) %>%
  filter(!is.na(coord1D)) %>%
  mutate(coord1D = case_when(
    
    legislatura == 52 ~ coord1D * -1,
    legislatura == 54 ~ coord1D * -1,
    legislatura == 55 ~ coord1D * -1,
    TRUE ~ coord1D
  )) %>%
  mutate(legislatura = paste0(legislatura, "ª")) %>%
  
  ggplot(aes(x = coord1D)) +
  geom_density(alpha = 0.9, fill = "gray", color = NA) +
  facet_wrap(~ legislatura) +
  tema() +
  scale_y_continuous(limits = c(0, 3.4), expand = c(0, 0)) +
  labs(x = "Pontos ideais (1º dimensão)", y = NULL)

ggsave(filename = here("outputs", "ideal_br_oc.pdf"), width = 6, height = 4)










