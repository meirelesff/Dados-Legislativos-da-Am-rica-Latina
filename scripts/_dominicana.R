# ---
# 0 COLETA DE VOTACOES NOMINAIS DA REPUBLICA DOMINICANA
# ---


# Pacotes
library(tidyverse)
library(rvest)
library(httr)
library(here)


## SCRAPER ---------------------------------------------------------------------


# Funcao para pegar proposicoes
pega_proposicoes <- function(){
  
  # Pega total de paginas
  pags <- "https://www.diputadosrd.gob.do/sil/api/iniciativa/getIniciativas?page=1&keyword=" %>%
    GET() %>%
    content() %>%
    pluck("total") %>%
    as.numeric()
  
  pags <- ceiling(pags / 10)
  
  # Pega as demais
  pega_props <- function(i){
    
    cli::cli_alert_info(i)
    
    "https://www.diputadosrd.gob.do/sil/api/iniciativa/getIniciativas?page={i}&keyword=" %>%
      glue::glue() %>%
      GET() %>%
      content() %>%
      pluck("results") %>%
      map(flatten) %>%
      map_df(as_tibble)
  }
  
  1:pags %>%
    map_df(pega_props)
}


## SCRAP -----------------------------------------------------------------------


props <- pega_proposicoes()
save(props, file = here("dados", "proposicoes_rep_dominicana.Rda"))
















