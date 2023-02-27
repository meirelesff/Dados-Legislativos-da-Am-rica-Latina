# ---
# 0 COLETA DE VOTACOES NOMINAIS DO MEXICO
# ---


# Pacotes
library(tidyverse)
library(rvest)
library(httr)
library(here)


## SCRAPER ---------------------------------------------------------------------


# Funcao para pegar todas as paginas de votacao
votacoes_mex <- function(){
  
  # Funcao interna para pegar links de votacoes
  pega_links <- function(url){
    
    cli::cli_alert_info(url)
    
    x <- GET(url)
    
    x %>%
      content(encoding = "UTF-8") %>%
      html_elements("li a") %>%
      html_attr("href") %>%
      str_subset("Gaceta")
  }
  
  x <- GET("http://gaceta.diputados.gob.mx/gp_votaciones.html")
  
  pags_votacoes <- content(x, encoding = "UTF-8") %>%
    html_elements("td td a") %>%
    html_attr("href") %>%
    paste0("http://gaceta.diputados.gob.mx", .) %>%
    map(pega_links)
  
  pags_votacoes %>%
    unlist() %>%
    paste0("http://gaceta.diputados.gob.mx", .) %>%
    str_subset("Votacio")
}


# Funcao para gerar requisicoes para cada tipo de resultado da votacao
forma_requisicoes <- function(url){
  
  cli::cli_alert_info(url)
  x <- GET(url)
  
  link <- content(x) %>%
    as.character() %>%
    str_match('action="\\s*(.*?)\\s*">') %>%
    pluck(2) %>%
    paste0("http://gaceta.diputados.gob.mx", .)
  
  nomes <- x %>%
    content() %>%
    html_elements("tr:nth-child(7) td:nth-child(2) center , tr:nth-child(6) td:nth-child(2) center , tr:nth-child(5) td:nth-child(2) center , tr:nth-child(4) td:nth-child(2) center , tr:nth-child(3) td:nth-child(2) center") %>%
    html_children() %>%
    html_attr("name")
  
  votos <- x %>%
    content() %>%
    html_elements("tr:nth-child(7) td:nth-child(2) center , tr:nth-child(6) td:nth-child(2) center , tr:nth-child(5) td:nth-child(2) center , tr:nth-child(4) td:nth-child(2) center , tr:nth-child(3) td:nth-child(2) center") %>%
    html_children() %>%
    html_attr("value")
  
  tibble(nomes = nomes, votos = votos) %>%
    mutate(tipo = c("Favor", "Contra", "Abstenção", "Quórum", "Ausente")) %>%
    mutate(votos = as.numeric(votos)) %>%
    filter(!is.na(nomes)) %>%
    mutate(nomes = str_remove_all(nomes, "lola\\[|\\]")) %>%
    mutate(body = str_match(url, "-\\s*(.*?)\\s*.php3")[, 2]) %>%
    mutate(body = glue::glue("evento={body}&lola%5B{nomes}%5D={votos}")) %>%
    mutate(referer = url) %>%
    mutate(url_req = link) 
}


# Funcao para pegar votacao
pega_votacao_mex <- function(url_req, referer, body){
  
  cli::cli_alert_info(referer)
  
  # Requisicao
  x <- POST(url_req,
            encode = "form",
            user_agent("Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:28.0) Gecko/20100101 Firefox/28.0"),
            add_headers(c(Referer = referer,
                          `Content-Type` = "application/x-www-form-urlencoded")),
            body = body
  )
  
  # Pega partidos
  partidos <- x %>%
    content() %>%
    html_elements("center") %>%
    html_text2() %>%
    str_replace("Diputados del", "Diputados de")
  
  partidos <- partidos[-length(partidos)] %>%
    str_match("Diputados de \\s*(.*?)\\s* que votaron")
  
  
  # Pega votos
  deps <- x %>%
    content() %>%
    html_elements("table:nth-child(3) td+ td") %>%
    html_table() %>%
    pluck(1)
  
  
  # Remove partidos que nao tiveram votos
  partidos <- partidos[deps$X1 != "", 2]
  
  deps %>%
    filter(X1 != "") %>%
    mutate(deps = paste0(X1, X2)) %>%
    select(deps) %>%
    mutate(partido = partidos) %>%
    mutate(deps = str_remove_all(deps, "[0-9]") %>% str_split(":")) %>%
    unnest(deps) %>%
    filter(deps != "") 
}

pega_votacao_mex_safe <- possibly(pega_votacao_mex, otherwise = NULL)


## SCRAP -----------------------------------------------------------------------


# Baixa os dados de votacoes
votacoes <- votacoes_mex()

# Forma as requisicoes
requisicoes <- map_df(votacoes, forma_requisicoes)

# Baixa as votacoes individuais
votos <- requisicoes %>%
  rowwise() %>%
  mutate(votos = list(pega_votacao_mex_safe(url_req, referer, body))) %>%
  ungroup()

save(votos, file = here("dados", "votos_mexico.rda"))








