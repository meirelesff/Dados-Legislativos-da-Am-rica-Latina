# ---
# 0 COLETA DE VOTACOES NOMINAIS DA GUATEMALA
# ---


# Pacotes
library(tidyverse)
library(rvest)
library(httr)
library(here)


## SCRAPER ---------------------------------------------------------------------


# Pega links de votacoes
pega_links <- function(){
  
  links <- "https://www.congreso.gob.gt/seccion_informacion_legislativa/votaciones_pleno" %>%
    GET(user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")) %>%
    content() %>%
    html_element("#asesores_detalle") %>%
    as.character()
  
  str_extract_all(links, "https:\\/\\/www\\.congreso\\.gob\\.gt\\/eventos_votaciones\\/\\d+", 
                  simplify = TRUE)[1, ]
}


x <- links[1] %>%
  GET(user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")) %>%
  content()

x %>%
  html_table() %>%
  pluck(1) %>%
  View()



## SCRAP -----------------------------------------------------------------------


links <- pega_links()




