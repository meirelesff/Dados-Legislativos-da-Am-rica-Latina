# ---
# 0 COLETA DE VOTACOES NOMINAIS DO PARAGUAI
# ---


# Pacotes
library(tidyverse)
library(rvest)
library(httr)
library(here)


## SCRAPER ---------------------------------------------------------------------


# Funcao para pegar proposicoes
pega_proposicoes <- function(){
  
  # Total de paginas
  total <- "http://datos.congreso.gov.py/opendata/api/data/proyecto/total" %>%
    GET() %>%
    content()
  
  pags <- ceiling(as.numeric(total) / 50)
  
  # Pega as proposicoes
  pega_props <- function(i){
    
    cli::cli_alert_info(i)
    
    "datos.congreso.gov.py/opendata/api/data/proyecto?offset={i}&limit=50" %>%
      glue::glue() %>%
      GET(user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")) %>%
      content() %>%
      map_df(as_tibble)
  }
  
  1:pags %>%
    map_df(pega_props)
}


# Funcao para pegar votacoes nominais


## SCRAP -----------------------------------------------------------------------

props <- pega_proposicoes()
save(props, file = here("dados", "proposicoes_paraguai.Rda"))








  
  