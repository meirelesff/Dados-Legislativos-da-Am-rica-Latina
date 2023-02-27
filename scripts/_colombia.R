# ---
# 0 COLETA DE VOTACOES NOMINAIS E PLS DA COLOMBIA (AGREGADO)
# ---


# Pacotes
library(tidyverse)
library(rvest)
library(httr)
library(here)


## SCRAPER ---------------------------------------------------------------------


# Pega agregados de votacoes nominais
pega_votacoes <- function(pag = 1, votacoes = 1000){
  
  cli::cli_alert_info(pag)
  
  x <- "https://congresovisible.uniandes.edu.co/votaciones/?page={pag}&rows={votacoes}" %>%
    glue::glue() %>%
    GET(user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")) %>%
    content()
  
  nome <- x %>%
    html_elements(".itemHeader a") %>%
    html_text()
  
  link <- x %>%
    html_elements(".itemHeader a") %>%
    html_attr("href") %>%
    paste0("https://congresovisible.uniandes.edu.co", .)
  
  datas <- x %>%
    html_elements(".itemFooter > div:nth-child(1)") %>%
    html_text() %>%
    lubridate::mdy()
  
  y <- x %>%
    html_elements(".bg-green p") %>%
    html_text() %>%
    as.numeric()
  
  n <- x %>%
    html_elements(".bg-red p") %>%
    html_text() %>%
    as.numeric()
  
  ausente <- x %>%
    html_elements(".bg-gray p") %>%
    html_text() %>%
    as.numeric()
  
  tibble(data = datas, texto = nome, link = link, 
         yes = if(pag == 1) y[-10] else y, 
         no = if(pag == 1) n[-10] else n, 
         ausente = if(pag == 1) ausente[-10] else ausente) 
}


# Pega leis
pega_leis <- function(pag = 1, leis = 100){
  
  cli::cli_alert_info(pag)
  
  x <- "https://congresovisible.uniandes.edu.co/proyectos-de-ley/?page={pag}&rows={leis}" %>%
    glue::glue() %>%
    GET(user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")) %>%
    content()
  
  nome <- x %>%
    html_elements(".itemHeader a") %>%
    html_text()
  
  link <- x %>%
    html_elements(".itemHeader a") %>%
    html_attr("href") %>%
    paste0("https://congresovisible.uniandes.edu.co/proyectos-de-ley/", .)
  
  n_senado <- x %>%
    html_elements(".with-pdb~ .with-pdb+ .with-pdb > p") %>%
    html_text()
  
  n_camara <- x %>%
    html_elements(".with-pdb:nth-child(2) > p") %>%
    html_text()
  
  autores <- x %>%
    html_elements(".title+ p") %>%
    html_text()
  
  tipo <- x %>%
    html_elements("li:nth-child(4)") %>%
    html_text() %>%
    str_remove("Tipo de proyecto:    ")
  
  cli::cli_alert_info("Pegando páginas individuais...")
  pags <- link %>%
    map(~ GET(.x) %>% content)
  
  datas <- pags %>%
    map(~ .x %>%
          html_elements(".fecha") %>%
          html_text() %>%
          lubridate::mdy() %>%
          min()
    )
  
  sinopse <- pags %>%
    map(~ .x %>%
          html_elements(".col-md-8 .TextoFormateado p") %>%
          html_text2() 
    )
  
  
  tibble(data = as.Date(as.numeric(datas), origin = "1970-01-01"), n_senado = n_senado, n_camara = n_camara, autores, tipo,
         texto = nome, link = link, sinopse = sinopse) 
}




## COLETA ----------------------------------------------------------------------


# Pega votacoes
votacoes_col <- 1:11 %>%
  map_df(pega_votacoes)

save(votacoes_col, file = here("dados", "resumo_votacoes_colombia.Rda"))


# Pega leis
leis_col <- 1:124 %>%
  map_df(pega_leis)

save(leis_col, file = here("dados", "proposicoes_colombia.Rda"))












